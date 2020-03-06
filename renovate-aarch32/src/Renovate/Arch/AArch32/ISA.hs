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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce ( coerce )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Word ( Word8, Word64 )


import qualified Dismantle.ASL.AArch32 as D
import qualified Data.Macaw.AArch32 as MA32
import qualified Data.Macaw.Memory as MM

import qualified Renovate as R

data TargetAddress = NoAddress
                   | AbsoluteAddress (R.ConcreteAddress MA32.AArch32)
  deriving (Eq, Ord, Show)

newtype Instruction a = I { unI :: D.AnnotatedInstruction a }
  deriving (Eq, Show)

type instance R.Instruction MA32.AArch32 = Instruction
type instance R.InstructionAnnotation MA32.AArch32 = TargetAddress
type instance R.RegisterType MA.AArch32 = Some D.Operand

instance PP.Pretty (Instruction a) where
  pretty = PP.pretty . armPrettyInstruction

instance Functior Instruction where
  fmap f (I i) =
    case i of
      D.Instruction opc operands ->
        I (D.Instruction (coerce opc) (FC.fmapFC (\(D.Annotated a o) -> D.Annotated (f a) o) operands))

data InstructionDisassemblyFailure =
  InstructionDisassemblyFailure BS.ByteString Int
  deriving (Show)

instance C.Exception InstructionDisassemblyFailure

assemble :: (C.MonadThrow m) => Instruction () -> m BS.ByteString
assemble = return . LBS.toStrict . D.assembleInstruction . toInst

disassemble :: (C.MonadThrow m) => BS.ByteString -> m (Int, Instruction ())
disassemble bs =
  case minsn of
    Just i -> return (bytesConsumed, fromInst i)
    Nothing -> C.throwM (InstructionDisassemblyFailure bs bytesConsumed)
  where
    (bytesConsumed, minsn) = D.disassembleInstruction (LBS.fromStrict bs)

armPrettyInstruction :: Instruction a -> String
armPrettyInstruction = show . D.ppInstruction . toInst

toInst :: Instruction a -> D.Instruction
toInst i =
  case unI i of
    D.Instruction opc annotatedOps ->
      D.Instruction (coerce opc) (FC.fmapFC unannotateOpcode annotatedOps)

fromInst :: D.Instruction -> Instruction ()
fromInst i =
  case i of
    D.Instruction opc unannotatedOps ->
      I (D.Instruction (coerce opc) (FC.fmapFC (D.Annotated ()) unannotatedOps))

unannotateOpcode :: D.Annotated a D.Operand tp -> D.Operand tp
unannotateOpcode (D.Annotated _ op) = op

armInstrSize :: Instruction a -> Word8
armInstrSize = error "instr sizes"

armMakePadding :: Word64 -> [Instruction ()]
armMakePadding nBytes = error "make padding"

armMakeRelativeJumpTo :: R.ConcreteAddress MA32.AArch32 -> R.ConcreteAddress MA32.AArch32 -> [Instruction ()]
armMakeRelativeJumpTo = error "make relative jump to"

armMaxRelativeJumpSize :: Word64
armMaxRelativeJumpSize = undefined

-- FIXME: This one will be tricky - I think we can simplify it a lot if we pass
-- in the macaw block containing the instruction.  If it isn't the entire block,
-- perhaps just the sequence of macaw statements corresponding to this
-- instruction (which we can obtain via metadata).  Ultimately, the problem is
-- that almost any instruction can be a jump if it writes directly to the PC,
-- and figuring that out requires deep semantic knowledge.
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
    D.Instruction opc operands ->
      I (D.Instruction (coerce opc) (FC.fmapFC (\(D.Annotated _ operand) -> D.Annotated () operand) operands))

armSymbolizeAddresses :: MM.Memory 32
                      -> (R.ConcreteAddress MA32.AArch32 -> Maybe (R.SymbolicAddress MA32.AArch32))
                      -> R.ConcreteAddress MA32.AArch32
                      -> Maybe (R.SymbolicAddress MA32.AArch32)
                      -> Instruction ()
                      -> [R.TaggedInstruction MA32.AArch32 TargetAddress]
armSymbolizeAddresses _mem _lookup _insnAddr mSymbolicTarget i =
  case unI i of
    D.Instruction opc operands ->
      let newInsn = D.Instruction (coerce opc) (FC.fmapFC annotateNull operands)
      in [R.tagInstruction mSymbolicTarget (I newInsn)]
  where
    annotateNull (D.Annotated _ operand) = D.Annotated NoAddress operand

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
