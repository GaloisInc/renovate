{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Renovate.Arch.AArch32.ISA (
  isa,
  assemble,
  disassemble,
  Instruction,
  TargetAddress(..),
  InstructionDisassemblyFailure(..)
  ) where

import qualified Control.Monad.Catch as C
import qualified Data.Bits as DB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce ( coerce )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Word ( Word8, Word64 )
import           Text.Printf ( printf )

-- NOTE: Renovate currently does not rewrite thumb blocks
--
-- The challenge there is that we would really like to be able to guarantee that
-- blocks only contain instructions from a single instruction set.  This API
-- makes that difficult to enforce
import qualified Dismantle.ARM.A32 as DA
import qualified Dismantle ARM.T32 as DT
import qualified Data.Macaw.AArch32 as MA32
import qualified Data.Macaw.Memory as MM

import qualified Renovate as R
import qualified Renovate.Arch.AArch32.Panic as RP

data TargetAddress = NoAddress
                   | AbsoluteAddress (R.ConcreteAddress MA32.AArch32)
  deriving (Eq, Ord, Show)

data Instruction a = AInstruction (DA.AnnotatedInstruction a)
                   | TInstruction (DT.AnnotatedInstruction a)
  deriving (Eq, Show)

type instance R.Instruction MA32.AArch32 = Instruction
type instance R.InstructionAnnotation MA32.AArch32 = TargetAddress
type instance R.RegisterType MA.AArch32 = Operand
-- The type of
data instance R.ArchRepr MA.AArch32 =

data Operand where
  AOperand :: DA.Operand tp -> Operand
  TOperand :: DT.Operand tp -> Operand

instance PP.Pretty (Instruction a) where
  pretty = PP.pretty . armPrettyInstruction

instance Functor Instruction where
  fmap f i =
    case i of
      AInstruction (DA.Instruction opc operands) ->
        I (DA.Instruction (coerce opc) (FC.fmapFC (\(DA.Annotated a o) -> DA.Annotated (f a) o) operands))
      TInstruction (DT.Instruction opc operands) ->
        I (DT.Instruction (coerce opc) (FC.fmapFC (\(DT.Annotated a o) -> DT.Annotated (f a) o) operands))

data InstructionDisassemblyFailure =
  InstructionDisassemblyFailure BS.ByteString Int
  deriving (Show)

instance C.Exception InstructionDisassemblyFailure

assemble :: (C.MonadThrow m) => Instruction () -> m BS.ByteString
assemble i =
  case i of
    AInstruction ai -> return (LBS.toStrict (DA.assembleInstruction ai))
    TInstruction ti -> return (LBS.toStrict (DT.assembleInstruction ti))

-- FIXME: Disassembly is actually a significant problem.  We don't know if we
-- should disassemble as ARM or Thumb.  We could try each, but that is no
-- guarantee.  We would have better success if we disassembled an entire (macaw)
-- block at a time.  Changing the API to take a ParsedBlock would help.  That is
-- still not exactly a guarantee, though the chance of a block decoding as
-- either ARM or Thumb (and having the right number of instructions) is quite
-- low.
--
-- We could additionally compare the disassembly against the Show output in the
-- instruction metadata.  That would significantly improve confidence at some
-- complexity cost.  Perhaps that would be a fallback if two decodings work?
--
-- It would require passing the whole block to the disassembler and
-- disassembling the whole thing at once, which is actually not a major problem.
disassemble :: (C.MonadThrow m) => BS.ByteString -> m (Int, Instruction ())
disassemble bs =
  case minsn of
    Just i -> return (bytesConsumed, fromInst i)
    Nothing -> C.throwM (InstructionDisassemblyFailure bs bytesConsumed)
  where
    (bytesConsumed, minsn) = DA.disassembleInstruction (LBS.fromStrict bs)

armPrettyInstruction :: Instruction a -> String
armPrettyInstruction i =
  case i of
    AInstruction ai -> show (DA.ppInstruction ai)
    TInstruction ti -> show (DT.ppInstruction ai)

-- toInst :: Instruction a -> DA.Instruction
-- toInst i =
--   case i of
--     AInstruction (DA.Instruction opc annotatedOps) ->
--       DA.Instruction (coerce opc) (FC.fmapFC unannotateOpcode annotatedOps)

-- fromInst :: DA.Instruction -> Instruction ()
-- fromInst i =
--   case i of
--     DA.Instruction opc unannotatedOps ->
--       I (DA.Instruction (coerce opc) (FC.fmapFC (DA.Annotated ()) unannotatedOps))

unannotateOpcode :: DA.Annotated a DA.Operand tp -> DA.Operand tp
unannotateOpcode (DA.Annotated _ op) = op

-- | All A32 instructions are 4 bytes
--
-- This will have to change to support thumb
armInstrSize :: Instruction a -> Word8
armInstrSize i =
  case assemble i of
    Nothing ->
      RP.panic RP.ARMISA "armInstrSize" [ "Could not assemble: " ++ armPrettyInstruction i
                                        ]
    Just bs -> fromIntegral (BS.length bs)

armMakePadding :: Word64 -> [Instruction ()]
armMakePadding nBytes
  | leftover == 0 = replicate nInsns (fromInst nopInsn)
  | otherwise = error (printf "Unexpected byte count (%d); only instruction-sized padding is supported" nBytes)
  where
    nopInsn = DA.Instruction DA.HLT_A1
    (nInsns, leftover) = fromIntegral nBytes `divMod` 4

armMakeRelativeJumpTo :: R.ConcreteAddress MA32.AArch32 -> R.ConcreteAddress MA32.AArch32 -> [Instruction ()]
armMakeRelativeJumpTo = error "make relative jump to"

-- FIXME: Max relative jump size depends on ARM mode vs Thumb mode
--
-- Pass in a block repr?
armMaxRelativeJumpSize :: Word64
armMaxRelativeJumpSize = DB.bit 25 - 4

-- FIXME: This one will be tricky - I think we can simplify it a lot if we pass
-- in the macaw block containing the instruction.  If it isn't the entire block,
-- perhaps just the sequence of macaw statements corresponding to this
-- instruction (which we can obtain via metadata).  Ultimately, the problem is
-- that almost any instruction can be a jump if it writes directly to the PC,
-- and figuring that out requires deep semantic knowledge.
--
-- We can just look at either the arch-update marker from macaw or, if there is
-- none, the post state of the terminator.  This means that we can avoid passing
-- in any instructions: we just need to pass in the post arch state and use the
-- instruction pointer value out of it
armJumpType :: Instruction a
            -> MM.Memory 32
            -> R.ConcreteAddress MA32.AArch32
            -> R.JumpType MA32.AArch32
armJumpType = error "jump type"

armModifyJumpTarget :: R.ConcreteAddress MA32.AArch32
                    -> R.ConcreteFallthrough MA32.AArch32 ()
                    -> Maybe [Instruction ()]
armModifyJumpTarget = error "modify jump target"

-- This is a no-op on ARM because it is a load/store architecture with no
-- arbitrary memory reference operands
armConcretizeAddresses :: MM.Memory 32
                       -> R.ConcreteAddress MA32.AArch32
                       -> Instruction TargetAddress
                       -> Instruction ()
armConcretizeAddresses _mem _addr i =
  case unI i of
    DA.Instruction opc operands ->
      I (DA.Instruction (coerce opc) (FC.fmapFC (\(DA.Annotated _ operand) -> DA.Annotated () operand) operands))

armSymbolizeAddresses :: MM.Memory 32
                      -> (R.ConcreteAddress MA32.AArch32 -> Maybe (R.SymbolicAddress MA32.AArch32))
                      -> R.ConcreteAddress MA32.AArch32
                      -> Maybe (R.SymbolicAddress MA32.AArch32)
                      -> Instruction ()
                      -> [R.TaggedInstruction MA32.AArch32 TargetAddress]
armSymbolizeAddresses _mem _lookup _insnAddr mSymbolicTarget i =
  case unI i of
    DA.Instruction opc operands ->
      let newInsn = DA.Instruction (coerce opc) (FC.fmapFC annotateNull operands)
      in [R.tagInstruction mSymbolicTarget (I newInsn)]
  where
    annotateNull (DA.Annotated _ operand) = DA.Annotated NoAddress operand

isa :: R.ISA MA32.AArch32
isa =
  R.ISA { R.isaInstructionSize = armInstrSize
        , R.isaPrettyInstruction = armPrettyInstruction
        , R.isaMakePAdding = armMakePadding
        , R.isaMakeRelativeJumpTo = armMakeRelativeJumpTo
        , R.isaMaxRelativeJumpSize = armMaxRelativeJumpSize
        , R.isaJumpType = armJumpType
        , R.isaModifyJumpTarget = armModifyJumpTarget
        , R.isaConcretizeAddresses = armConcretizeAddresses
        , R.isaSymbolizeAddresses = armSymbolizeAddresses
        }
