{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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
import qualified Data.Parameterized.List as PL
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Word ( Word8, Word64 )
import qualified Data.Word.Indexed as W

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
data ARMReg (tp :: MT.Type) where
  ARMReg :: Int -> ARMReg (MT.BVType 32)
type instance MC.RegAddrWidth ARMReg = 32
type instance MC.ArchReg AArch32 = ARMReg
instance MC.RegisterInfo ARMReg where
instance ShowF ARMReg where
  showF (ARMReg r) = "r" ++ show r
instance Show (ARMReg tp) where
  show = showF
instance MT.HasRepr ARMReg MT.TypeRepr where
  typeRepr (ARMReg _) = MT.BVTypeRepr (MT.knownNat @32)

instance TestEquality ARMReg where
  testEquality (ARMReg r1) (ARMReg r2)
    | r1 == r2 = Just Refl
    | otherwise = Nothing

instance OrdF ARMReg where
  compareF (ARMReg r1) (ARMReg r2)
    | r1 == r2 = EQF
    | otherwise = fromOrdering (compare r1 r2)

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

pattern AI i <- ARMInstruction (armDropAnnotations -> i)
pattern TI i <- ThumbInstruction (thumbDropAnnotations -> i)

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
armMakeRelativeJumpTo src dest repr =
  case repr of
    ArchRepr A32Repr ->
      let off = fromIntegral ((src `R.addressDiff` dest) `DB.shiftR` 2)
      in singleton (DA.Instruction DA.B_A1 (DA.Bv4 0 DA.:< DA.Bv24 off DA.:< DA.Nil))
    ArchRepr T32Repr ->
      RP.panic RP.ARMISA "armMakeRelativeJumpTo" [ "Thumb rewriting is not yet supported"
                                                 ]

armMaxRelativeJumpSize :: R.InstructionArchRepr AArch32 tp -> Word64
armMaxRelativeJumpSize repr =
  case repr of
    ArchRepr A32Repr -> DB.bit 25 - 4
    ArchRepr T32Repr -> DB.bit 10 - 4

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
armJumpType i mem insnAddr pb =
  case asParsedTerminator insnAddr pb of
    Nothing -> Some R.NoJump
    Just term ->
      case term of
        -- Calls are really only issued using the standard instructions for that.
        --
        -- bl, blx
        --
        -- These are special because they populate the link register.
        --
        -- FIXME: Implement thumb cases
        MD.ParsedCall _regs _retLoc ->
          case i of
            AI (DA.Instruction DA.BL_i_A1 (DA.Bv4 _cond DA.:< DA.Bv24 off DA.:< DA.Nil)) ->
              Some (R.DirectCall insnAddr (fromIntegral (off `DB.shiftL` 2)))
            AI (DA.Instruction DA.BL_i_A2 (DA.Bv1 _ DA.:< DA.Bv4 _ DA.:< DA.Bv24 off DA.:< DA.Nil)) ->
              Some (R.DirectCall insnAddr (fromIntegral (off `DB.shiftL` 2)))
            AI (DA.Instruction DA.BLX_r_A1 (DA.Bv4 _cond DA.:< DA.Bv4 _reg DA.:< DA.QuasiMask12 _ DA.:< DA.Nil)) ->
              Some R.IndirectCall
            _ -> Some (R.NotInstrumentable insnAddr)
        -- PLT stubs won't be encountered in renovate (note: this terminator is
        -- a terminator of a PLT stub, which is not something we would rewrite,
        -- rather than a call to a PLT stub)
        MD.PLTStub {} -> Some R.IndirectCall
        -- The ParsedJump constructor is slightly special in macaw.  It could
        -- represent a direct jump or be synthesized for control flow
        -- fallthrough.
        --
        -- We will catch the cases where we can.
        --
        -- The fallthrough case is simple: if the target is the next address, it
        -- is a fallthrough and we will emit a 'NoJump'.  Otherwise it is a
        -- non-local unconditional jump.  If it is a recognized form, we will
        -- tag it appropriately.  Otherwise it is a jump that we can't rewrite.
        MD.ParsedJump _regs tgtSegOff ->
          if | Just tgtAddr <- R.concreteFromSegmentOff mem tgtSegOff
             , nextInsnAddr == tgtAddr -> Some R.NoJump
             | otherwise ->
               case i of
                 AI (DA.Instruction DA.B_A1 (DA.Bv4 _cond DA.:< DA.Bv24 off DA.:< DA.Nil)) ->
                   Some (R.RelativeJump R.Unconditional insnAddr (fromIntegral (off `DB.shiftL` 2)))
                 -- FIXME: Handle Thumb cases
                 _ -> Some (R.NotInstrumentable insnAddr)
        -- Any instruction in ARM can be a conditional jump due to predication...
        --
        -- We are only handling B with condition codes here.
        MD.ParsedBranch {} ->
          case i of
            AI (DA.Instruction DA.B_A1 (DA.Bv4 _cond DA.:< DA.Bv24 off DA.:< DA.Nil)) ->
              Some (R.RelativeJump R.Conditional insnAddr (fromIntegral (off `DB.shiftL` 2)))
            -- FIXME: Handle T32 cases
            _ -> Some (R.NotInstrumentable insnAddr)
        MD.ParsedLookupTable _regs _cond _tgts ->
          -- NOTE: Macaw won't identify a conditional indirect jump as a jump
          -- table, so these are always unconditional until that is made more
          -- general.  macaw-refinement could identify one...
          Some (R.IndirectJump R.Unconditional)
        MD.ParsedReturn _regs ->
          -- FIXME: For this case, would it be easier to identify conditionality
          -- based on the value of the IP, or based on inspection of the
          -- predication flag of the instruction?
          Some (R.Return R.Unconditional)
        MD.ParsedArchTermStmt _archTerm _regs _mret ->
          -- FIXME: This might not be right.  We might not have any arch
          -- terms except for svc, though, which would be fine
          Some R.IndirectCall
        MD.ParsedTranslateError msg ->
          RP.panic RP.ARMISA "armJumpType" [ "ParsedTranslateError should be analyzed by renovate:"
                                           , "  Reason: " ++ show msg
                                           ]
        MD.ClassifyFailure _regs msgs ->
          RP.panic RP.ARMISA "armJumpType" ( "ClassifyFailure should not be analyzed by renovate:"
                                           : msgs
                                           )
  where
    nextInsnAddr = insnAddr `R.addressAddOffset` fromIntegral (armInstrSize i)

data CurrentInstruction = NoInstruction
                        | InInstruction (MC.ArchAddrWord AArch32)
                        | FoundTarget

-- | If the given address corresponds to the block terminator, return that
-- terminator.
--
-- This can panic if there is a major inconsistency
--
-- Scan through an open an active instruction at the 'MD.InstructionStart'
-- statement and close it at 'MD.ArchState'
asParsedTerminator :: R.ConcreteAddress AArch32
                   -> MD.ParsedBlock AArch32 ids
                   -> Maybe (MD.ParsedTermStmt AArch32 ids)
asParsedTerminator insnAddr pb =
  case foldr searchTarget NoInstruction (MD.pblockStmts pb) of
    NoInstruction -> Nothing
    InInstruction addr
      | insnAddr == R.concreteFromAbsolute addr -> Just (MD.pblockTermStmt pb)
      | otherwise -> Nothing
    FoundTarget ->
      -- If we found a 'MD.ArchState' corresponding to our target, it is a
      -- non-terminator instruction
      Nothing
  where
    searchTarget stmt curState =
      case curState of
        FoundTarget -> FoundTarget
        _ ->
          case stmt of
            MC.InstructionStart addr _ -> InInstruction addr
            MC.ArchState addr _
              | Just caddrWord <- MM.asAbsoluteAddr addr
              , insnAddr == R.concreteFromAbsolute caddrWord -> FoundTarget
              | otherwise -> NoInstruction
            _ -> curState

-- | FIXME: We may have to use the long jump strategy from PowerPC here
armModifyJumpTarget :: R.ConcreteAddress AArch32
                    -> Instruction tp ()
                    -> R.RelocatableTarget AArch32 R.ConcreteAddress R.HasSomeTarget
                    -> Maybe (DLN.NonEmpty (Instruction tp ()))
armModifyJumpTarget insnAddr i0 (R.RelocatableTarget newTarget) =
  case i0 of
    ARMInstruction i ->
      case armDropAnnotations i of
        DA.Instruction DA.B_A1 (DA.Bv4 cond DA.:< DA.Bv24 _off DA.:< DA.Nil) ->
          -- FIXME: Assert in range
          let newOff = fromIntegral ((insnAddr `R.addressDiff` newTarget) `DB.shiftR` 2)
          in Just $ singleton (DA.Instruction DA.B_A1 (DA.Bv4 cond DA.:< DA.Bv24 newOff DA.:< DA.Nil))
        DA.Instruction DA.BL_i_A1 (DA.Bv4 cond DA.:< DA.Bv24 _off DA.:< DA.Nil) ->
          let newOff = fromIntegral ((insnAddr `R.addressDiff` newTarget) `DB.shiftR` 2)
          in Just $ singleton (DA.Instruction DA.B_A1 (DA.Bv4 cond DA.:< DA.Bv24 newOff DA.:< DA.Nil))
        DA.Instruction DA.BL_i_A2 (DA.Bv1 b1 DA.:< DA.Bv4 cond DA.:< DA.Bv24 _off DA.:< DA.Nil) ->
          let newOff = fromIntegral ((insnAddr `R.addressDiff` newTarget) `DB.shiftR` 2)
          in Just $ singleton (DA.Instruction DA.BL_i_A2 (DA.Bv1 b1 DA.:< DA.Bv4 cond DA.:< DA.Bv24 newOff DA.:< DA.Nil))
        _ ->
          RP.panic RP.ARMISA "armModifyJumpTarget" [ "Encountered unmodifiable instruction that should not have reached here"
                                                   , "  " ++ armPrettyInstruction i0
                                                   ]
    ThumbInstruction {} ->
      RP.panic RP.ARMISA "armModifyJumpTarget" [ "Thumb rewriting is not yet supported"
                                               ]

singleton :: DA.Instruction -> DLN.NonEmpty (Instruction A32 ())
singleton = (DLN.:| []) . ARMInstruction . toAnnotatedARM

armConcretizeAddresses :: MM.Memory 32
                       -> R.ConcreteAddress AArch32
                       -> Instruction tp TargetAddress
                       -> Instruction tp ()
armConcretizeAddresses _mem insnAddr i =
  case i of
    ARMInstruction (DA.Instruction opc operands) ->
      case opc of
        DA.LDRT_A1 ->
          case operands of
            rN DA.:< rt DA.:< u DA.:< cond DA.:< DA.Annotated (AbsoluteAddress absAddr) _off12 DA.:< DA.Nil ->
              let newOff14 = fromIntegral (insnAddr `R.addressDiff` absAddr)
                  newOff12 = newOff14 `DB.shiftR` 2
                  operands' = (      rN
                               DA.:< rt
                               DA.:< u
                               DA.:< cond
                               DA.:< DA.Annotated NoAddress (DA.Bv12 newOff12)
                               DA.:< DA.Nil
                              )
                  i' = DA.Instruction (coerce opc) (FC.fmapFC toUnitAnnotation operands')
              in ARMInstruction i'
            _ -> ARMInstruction (DA.Instruction (coerce opc) (FC.fmapFC toUnitAnnotation operands))
        _ -> ARMInstruction (DA.Instruction (coerce opc) (FC.fmapFC toUnitAnnotation operands))
    ThumbInstruction {} ->
      RP.panic RP.ARMISA "armConcretizeAddresses" [ "Thumb rewriting is not yet support" ]
  where
    toUnitAnnotation :: forall tp
                      . DA.Annotated TargetAddress DA.Operand tp
                     -> DA.Annotated () DA.Operand tp
    toUnitAnnotation (DA.Annotated _ op) = DA.Annotated () op

armSymbolizeAddresses :: MM.Memory 32
                      -> R.ConcreteAddress AArch32
                      -> Maybe (R.SymbolicAddress AArch32)
                      -> Instruction tp ()
                      -> [R.TaggedInstruction AArch32 tp TargetAddress]
armSymbolizeAddresses _mem insnAddr mSymbolicTarget i =
  case i of
    ARMInstruction (armDropAnnotations -> DA.Instruction opc operands) ->
      case opc of
        DA.LDRT_A1 ->
          case operands of
            -- LDRT_A1 :: Opcode o '["Bv4", "Bv4", "Bv1", "Bv4", "Bv12"]
            --
            -- Rn, Rt, U, cond, imm12
            --
            -- NOTE: So far, all PC-relative address computation in gcc-compiled
            -- binaries seems to be done using this instruction
            DA.Bv4 rN DA.:< rt DA.:< u DA.:< cond DA.:< DA.Bv12 off12 DA.:< DA.Nil
              | isPC rN ->
                let off14 = off12 `DB.shiftL` 2
                    target = insnAddr `R.addressAddOffset` fromIntegral off14
                    i' = DA.Instruction (coerce opc) (     noAddr (DA.Bv4 rN)
                                                     DA.:< noAddr rt
                                                     DA.:< noAddr u
                                                     DA.:< noAddr cond
                                                     DA.:< withAddr target (DA.Bv12 off12)
                                                     DA.:< DA.Nil
                                                     )
                in [R.tagInstruction mSymbolicTarget (ARMInstruction i')]
              | otherwise -> toATagged opc operands
        _ -> toATagged opc operands
    ThumbInstruction {} ->
      RP.panic RP.ARMISA "armConcretizeAddresses" [ "Thumb rewriting is not yet support" ]
  where
    annotateNull :: forall tp . DA.Operand tp -> DA.Annotated TargetAddress DA.Operand tp
    annotateNull operand = DA.Annotated NoAddress operand
    toATagged :: forall sh
               . DA.Opcode DA.Operand sh
              -> PL.List DA.Operand sh
              -> [R.TaggedInstruction AArch32 A32 TargetAddress]
    toATagged opc operands =
      let newInsn = DA.Instruction (coerce opc) (FC.fmapFC annotateNull operands)
      in [R.tagInstruction mSymbolicTarget (ARMInstruction newInsn)]

noAddr :: DA.Operand tp -> DA.Annotated TargetAddress DA.Operand tp
noAddr = DA.Annotated NoAddress

withAddr :: R.ConcreteAddress AArch32 -> DA.Operand tp -> DA.Annotated TargetAddress DA.Operand tp
withAddr t = DA.Annotated (AbsoluteAddress t)

isPC :: W.W 4 -> Bool
isPC i = i == 15

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
