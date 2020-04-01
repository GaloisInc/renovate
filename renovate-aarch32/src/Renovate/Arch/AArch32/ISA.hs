{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Renovate.Arch.AArch32.ISA (
  AArch32, -- FIXME: temporary
  isa,
  assemble,
  disassemble,
  Instruction,
  TargetAddress(..),
  InstructionDisassemblyFailure(..),
  ARMRepr(..),
  R.InstructionArchRepr(ArchRepr),
  A32,
  T32
  ) where

import qualified Control.Monad.Catch as C
import qualified Data.Bits as DB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce ( coerce )
import qualified Data.List.NonEmpty as DLN
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Word ( Word8, Word64 )

-- NOTE: Renovate currently does not rewrite thumb blocks
--
-- The challenge there is that we would really like to be able to guarantee that
-- blocks only contain instructions from a single instruction set.  This API
-- makes that difficult to enforce
import qualified Dismantle.ARM.A32 as DA
import qualified Dismantle.ARM.T32 as DT
-- import qualified Data.Macaw.AArch32 as MA32
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Types as MT

import qualified Renovate as R
import qualified Renovate.Arch.AArch32.Panic as RP

-- FIXME: Pull this from macaw/semmc instead
data AArch32
data ARMReg (tp :: MT.Type) = ARMReg Int
type instance MC.RegAddrWidth ARMReg = 32
type instance MC.ArchReg AArch32 = ARMReg
-- END temporary definitions

data ARMKind = A32 | T32
type A32 = 'A32
type T32 = 'T32

type instance R.InstructionArchReprKind AArch32 = ARMKind

data ARMRepr tp where
  A32Repr :: ARMRepr A32
  T32Repr :: ARMRepr T32

data instance R.InstructionArchRepr AArch32 tp = ArchRepr (ARMRepr tp)

instance TestEquality ARMRepr where
  testEquality A32Repr A32Repr = Just Refl
  testEquality T32Repr T32Repr = Just Refl
  testEquality _ _ = Nothing

instance TestEquality (R.InstructionArchRepr AArch32) where
  testEquality (ArchRepr r1) (ArchRepr r2) = do
    Refl <- testEquality r1 r2
    return Refl

data TargetAddress = NoAddress
                   | AbsoluteAddress (R.ConcreteAddress AArch32)
  deriving (Eq, Ord, Show)

data Instruction tp a where
  ARMInstruction :: DA.AnnotatedInstruction a -> Instruction A32 a
  ThumbInstruction :: DT.AnnotatedInstruction a -> Instruction T32 a

instance Show (Instruction tp a) where
  show i = show (PP.pretty i)

type instance R.Instruction AArch32 = Instruction
type instance R.InstructionAnnotation AArch32 = TargetAddress
type instance R.RegisterType AArch32 = Operand

-- | ARM operands
--
-- The quantified @atp@ parameter is the type-level symbol attached to dismantle
-- operands.  The renovate-level operand only tracks the architecture dialect.
data Operand tp where
  ARMOperand :: DA.Operand atp -> Operand A32
  ThumbOperand :: DT.Operand atp -> Operand T32

instance PP.Pretty (Instruction tp a) where
  pretty = PP.pretty . armPrettyInstruction

instance Functor (Instruction tp) where
  fmap f i =
    case i of
      ARMInstruction (DA.Instruction opc operands) ->
        ARMInstruction (DA.Instruction (coerce opc) (FC.fmapFC (\(DA.Annotated a o) -> DA.Annotated (f a) o) operands))
      ThumbInstruction (DT.Instruction opc operands) ->
        ThumbInstruction (DT.Instruction (coerce opc) (FC.fmapFC (\(DT.Annotated a o) -> DT.Annotated (f a) o) operands))

data InstructionDisassemblyFailure =
  InstructionDisassemblyFailure LBS.ByteString Int
  | EmptyBlock (R.ConcreteAddress AArch32)
  | OverlappingBlocks (R.ConcreteAddress AArch32) (R.ConcreteAddress AArch32)
  deriving (Show)

instance C.Exception InstructionDisassemblyFailure

assemble :: (C.MonadThrow m) => Instruction tp () -> m BS.ByteString
assemble i =
  case i of
    ARMInstruction ai -> return (LBS.toStrict (DA.assembleInstruction (armDropAnnotations ai)))
    ThumbInstruction ti -> return (LBS.toStrict (DT.assembleInstruction (thumbDropAnnotations ti)))

-- | Disassemble a concrete block from a bytestring
--
-- This is very trick in ARM because we aren't passing in the expected decoding
-- mode (because renovate's recovery has no way of knowing).  We attempt to
-- disassemble the requested byte range as A32 first.  If that fails, we attempt
-- to disassemble as T32.
--
-- FIXME: We could make this safer by comparing the recovered instructions
-- against the instruction start metadata in the macaw 'MD.ParsedBlock', though
-- doing so is tough because it is just string matching.
disassemble :: forall m ids
             . (C.MonadThrow m)
            => MD.ParsedBlock AArch32 ids
            -> R.ConcreteAddress AArch32
            -> R.ConcreteAddress AArch32
            -> BS.ByteString
            -> m (R.ConcreteBlock AArch32)
disassemble pb startAddr endAddr bs0 = do
  let acon = ARMInstruction . toAnnotatedARM
  let minsns0 = go A32Repr acon DA.disassembleInstruction 0 startAddr (LBS.fromStrict bs0) []
  case DLN.nonEmpty =<< minsns0 of
    Just insns -> return (R.concreteBlock startAddr insns (ArchRepr A32Repr) pb)
    Nothing -> do
      let tcon = ThumbInstruction . toAnnotatedThumb
      minsns1 <- go T32Repr tcon DT.disassembleInstruction 0 startAddr (LBS.fromStrict bs0) []
      case DLN.nonEmpty minsns1 of
        Nothing -> C.throwM (EmptyBlock startAddr)
        Just insns -> return (R.concreteBlock startAddr insns (ArchRepr T32Repr) pb)
  where
    go :: forall tp i m'
        . (C.MonadThrow m')
       => ARMRepr tp
       -> (i -> Instruction tp ())
       -> (LBS.ByteString -> (Int, Maybe i))
       -> Int
       -> R.ConcreteAddress AArch32
       -> LBS.ByteString
       -> [Instruction tp ()]
       -> m' [Instruction tp ()]
    go repr con dis totalRead insnAddr b insns =
      case dis b of
        (bytesRead, Nothing) ->
          C.throwM (InstructionDisassemblyFailure b bytesRead)
        (bytesRead, Just i) ->
          let nextAddr = insnAddr `R.addressAddOffset` fromIntegral bytesRead
          in if | nextAddr > endAddr -> C.throwM (OverlappingBlocks startAddr endAddr)
                | nextAddr == endAddr ->
                  return (reverse (con i : insns))
                | otherwise ->
                  let b' = LBS.drop (fromIntegral bytesRead) b
                  in go repr con dis (totalRead + bytesRead) nextAddr b' (con i : insns)

armPrettyInstruction :: Instruction tp a -> String
armPrettyInstruction i =
  case i of
    ARMInstruction ai -> show (DA.ppInstruction (armDropAnnotations ai))
    ThumbInstruction ti -> show (DT.ppInstruction (thumbDropAnnotations ti))

armDropAnnotations :: DA.AnnotatedInstruction a -> DA.Instruction
armDropAnnotations i =
  case i of
    DA.Instruction opc annotatedOps ->
      DA.Instruction (coerce opc) (FC.fmapFC armUnannotateOpcode annotatedOps)

toAnnotatedARM :: DA.Instruction -> DA.AnnotatedInstruction ()
toAnnotatedARM i =
  case i of
    DA.Instruction opc ops ->
      DA.Instruction (coerce opc) (FC.fmapFC (DA.Annotated ()) ops)

toAnnotatedThumb :: DT.Instruction -> DT.AnnotatedInstruction ()
toAnnotatedThumb i =
  case i of
    DT.Instruction opc ops ->
      DT.Instruction (coerce opc) (FC.fmapFC (DT.Annotated ()) ops)

thumbDropAnnotations :: DT.AnnotatedInstruction a -> DT.Instruction
thumbDropAnnotations i =
  case i of
    DT.Instruction opc annotatedOps ->
      DT.Instruction (coerce opc) (FC.fmapFC thumbUnannotateOpcode annotatedOps)

armUnannotateOpcode :: DA.Annotated a DA.Operand tp -> DA.Operand tp
armUnannotateOpcode (DA.Annotated _ op) = op

thumbUnannotateOpcode :: DT.Annotated a DT.Operand tp -> DT.Operand tp
thumbUnannotateOpcode (DT.Annotated _ op) = op

-- | All A32 instructions are 4 bytes
--
-- Thumb instructions can be 2 or 4 bytes, so we re-assemble them to find the
-- real size.
armInstrSize :: Instruction tp a -> Word8
armInstrSize i =
  case i of
    ARMInstruction {} -> 4
    ThumbInstruction ti ->
      let bytes = DT.assembleInstruction (thumbDropAnnotations ti)
      in fromIntegral (LBS.length bytes)

-- | If we are making a block for some arbitrary purpose, we will use the ARM
-- architecture.  If there is a more specific need, the caller should provide
-- their own repr.
armDefaultInstructionArchRepr :: R.SomeInstructionArchRepr AArch32
armDefaultInstructionArchRepr = R.SomeInstructionArchRepr (ArchRepr A32Repr)

armMakePadding :: Word64 -> R.InstructionArchRepr AArch32 tp -> [Instruction tp ()]
armMakePadding nBytes repr =
  case repr of
    ArchRepr A32Repr
      | leftoverARM == 0 ->
        fmap (ARMInstruction . toAnnotatedARM) (replicate (fromIntegral nARMInsns) aBrk)
      | otherwise ->
        RP.panic RP.ARMISA "armMakePadding" [ "Unexpected byte count (A32): " ++ show nBytes
                                            , "Only instruction-sized padding (4 bytes) is supported"
                                            ]
    ArchRepr T32Repr
      | leftoverThumb == 0 ->
        fmap (ThumbInstruction . toAnnotatedThumb) (replicate (fromIntegral nThumbInsns) tBrk)
      | otherwise ->
        RP.panic RP.ARMISA "armMakePadding" [ "Unexpected byte count (T32): " ++ show nBytes
                                            , "Only instruction-sized padding (2 bytes) is supported"
                                            ]
  where
    aBrk :: DA.Instruction
    aBrk = DA.Instruction DA.BKPT_A1 (DA.Bv4 0 DA.:< DA.Bv12 0 DA.:< DA.Bv4 0 DA.:< DA.Nil)
    tBrk :: DT.Instruction
    tBrk = DT.Instruction DT.BKPT_T1 (DT.Bv8 0 DT.:< DT.Nil)
    (nARMInsns, leftoverARM) = nBytes `divMod` 4
    (nThumbInsns, leftoverThumb) = nBytes `divMod` 2

armMakeRelativeJumpTo :: R.ConcreteAddress AArch32
                      -> R.ConcreteAddress AArch32
                      -> R.InstructionArchRepr AArch32 tp
                      -> DLN.NonEmpty (Instruction tp ())
armMakeRelativeJumpTo = error "make relative jump to"

-- FIXME: Max relative jump size depends on ARM mode vs Thumb mode
--
-- Pass in a block repr?
armMaxRelativeJumpSize :: R.InstructionArchRepr AArch32 tp -> Word64
armMaxRelativeJumpSize repr =
  case repr of
    ArchRepr A32Repr -> DB.bit 25 - 4
    ArchRepr T32Repr -> DB.bit 11 - 4

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
armJumpType :: Instruction tp a
            -> MM.Memory 32
            -> R.ConcreteAddress AArch32
            -> MD.ParsedBlock AArch32 ids
            -> Some (R.JumpType AArch32)
armJumpType = error "jump type"

armModifyJumpTarget :: R.ConcreteAddress AArch32
                    -> Instruction tp ()
                    -> R.RelocatableTarget AArch32 R.ConcreteAddress R.HasSomeTarget
                    -> Maybe (DLN.NonEmpty (Instruction tp ()))
armModifyJumpTarget = error "modify jump target"

-- This is a no-op on ARM because it is a load/store architecture with no
-- arbitrary memory reference operands
armConcretizeAddresses :: MM.Memory 32
                       -> R.ConcreteAddress AArch32
                       -> Instruction tp TargetAddress
                       -> Instruction tp ()
armConcretizeAddresses _mem _addr i =
  case i of
    ARMInstruction (DA.Instruction opc operands) ->
      ARMInstruction (DA.Instruction (coerce opc) (FC.fmapFC (\(DA.Annotated _ operand) -> DA.Annotated () operand) operands))
    ThumbInstruction (DT.Instruction opc operands) ->
      ThumbInstruction (DT.Instruction (coerce opc) (FC.fmapFC (\(DT.Annotated _ operand) -> DT.Annotated () operand) operands))

armSymbolizeAddresses :: MM.Memory 32
                      -> R.ConcreteAddress AArch32
                      -> Maybe (R.SymbolicAddress AArch32)
                      -> Instruction tp ()
                      -> [R.TaggedInstruction AArch32 tp TargetAddress]
armSymbolizeAddresses _mem _insnAddr mSymbolicTarget i =
  case i of
    ARMInstruction (DA.Instruction opc operands) ->
      let newInsn = DA.Instruction (coerce opc) (FC.fmapFC annotateNull operands)
      in [R.tagInstruction mSymbolicTarget (ARMInstruction newInsn)]
  where
    annotateNull :: forall x tp . DA.Annotated x DA.Operand tp -> DA.Annotated TargetAddress DA.Operand tp
    annotateNull (DA.Annotated _ operand) = DA.Annotated NoAddress operand

isa :: R.ISA AArch32
isa =
  R.ISA { R.isaInstructionSize = armInstrSize
        , R.isaPrettyInstruction = armPrettyInstruction
        , R.isaMakePadding = armMakePadding
        , R.isaDefaultInstructionArchRepr = armDefaultInstructionArchRepr
        , R.isaMakeRelativeJumpTo = armMakeRelativeJumpTo
        , R.isaMaxRelativeJumpSize = armMaxRelativeJumpSize
        , R.isaJumpType = armJumpType
        , R.isaModifyJumpTarget = armModifyJumpTarget
        , R.isaConcretizeAddresses = armConcretizeAddresses
        , R.isaSymbolizeAddresses = armSymbolizeAddresses
        }
