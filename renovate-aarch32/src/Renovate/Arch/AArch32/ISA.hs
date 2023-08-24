{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Renovate.Arch.AArch32.ISA (
  A32.AArch32,
  isa,
  assemble,
  disassemble,
  Instruction,
  InstructionDisassemblyFailure(..),
  ARMRepr(..),
  A32,
  T32
  ) where

import Debug.Trace (trace)

import qualified Control.Monad.Catch as C
import qualified Data.Bits as DB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce ( coerce )
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import           Data.Maybe ( isJust )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.NatRepr as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Void ( Void )
import           Data.Word ( Word8, Word64 )
import qualified Data.Word.Indexed as W
import           GHC.TypeNats ( KnownNat )

-- NOTE: Renovate currently does not rewrite thumb blocks
--
-- The challenge there is that we would really like to be able to guarantee that
-- blocks only contain instructions from a single instruction set.  This API
-- makes that difficult to enforce
import qualified Dismantle.ARM.A32 as DA
import qualified Dismantle.ARM.T32 as DT
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.ARM as MA
import qualified SemMC.Architecture.AArch32 as A32

import qualified Renovate as R
import qualified Renovate.Arch.AArch32.Panic as RP
import           Renovate.Arch.AArch32.Repr

-- | A wrapper around A32 and T32 instructions type indexed to ensure that ARM
-- and Thumb instructions cannot be mixed in a single basic block
data Instruction tp a where
  -- | An A32 encoded instruction.
  --   We have separate constructors for annotated vs. unannotated instructions
  --   to indicate that instructions with relocation information have to 
  --   include a tag to indicate whether or not the instruction was considered
  --   a jump in the original block.
  --   This is used to implement 'isaIsRelocatableJump', which needs to be able to decide
  --   if an instruction is a jump independent of its surrounding context
  ARMInstructionBare :: DA.AnnotatedInstruction () -> Instruction A32 ()
  -- | Flag indicates that the instruction was a jump in the original block
  ARMInstructionAnn :: DA.AnnotatedInstruction (R.Relocation MA.ARM) -> Bool -> Instruction A32 (R.Relocation MA.ARM)
  -- | Raw bytes in the A32 instruction stream
  ARMBytes :: BS.ByteString -> Instruction A32 a
  -- | A T32 instruction
  ThumbInstruction :: DT.AnnotatedInstruction a -> Instruction T32 a

asArmInstruction :: Instruction tp a -> Maybe (DA.AnnotatedInstruction a, tp PN.:~: A32)
asArmInstruction i = case i of
  ARMInstructionBare i' -> Just $ (i', PN.Refl)
  ARMInstructionAnn i' _ -> Just $ (i', PN.Refl)
  _ -> Nothing

pattern ARMInstruction :: forall (tp :: ARMKind) a
            . ()
           => (tp ~ A32)
           => DA.AnnotatedInstruction a -> Instruction tp a
pattern ARMInstruction i <- (asArmInstruction -> Just (i,PN.Refl))

{-#COMPLETE ARMInstruction, ARMBytes, ThumbInstruction #-}

instance Show (Instruction tp a) where
  show i = show (PP.pretty i)

armInstructionRepr :: Instruction tp a -> ARMRepr tp
armInstructionRepr i =
  case i of
    ARMInstruction {} -> A32Repr
    ARMBytes {} -> A32Repr
    ThumbInstruction {} -> T32Repr

pattern AI :: forall (tp :: ARMKind) a
            . ()
           => (tp ~ A32)
           => DA.Instruction -> Instruction tp a
pattern AI i <- ARMInstruction (armDropAnnotations -> i)

-- pattern TI :: forall (tp :: ARMKind) a
--             . ()
--            => (tp ~ T32)
--            => DT.Instruction -> Instruction tp a
-- pattern TI i <- ThumbInstruction (thumbDropAnnotations -> i)

type instance R.Instruction MA.ARM = Instruction
type instance R.ArchitectureRelocation MA.ARM = Void
type instance R.RegisterType MA.ARM = Operand


-- | ARM operands
--
-- The quantified @atp@ parameter is the type-level symbol attached to dismantle
-- operands.  The renovate-level operand only tracks the architecture dialect.
data Operand tp where
  ARMOperand :: DA.Operand atp -> Operand A32
  ThumbOperand :: DT.Operand atp -> Operand T32

instance PP.Pretty (Instruction tp a) where
  pretty = PP.pretty . armPrettyInstruction

-- We've restricted the set of valid annotations, and so the instruction is no
-- longer a functor
{-
instance Functor (Instruction tp) where
  fmap f i =
    case i of
      ARMInstruction (DA.Instruction opc operands) ->
        ARMInstruction (DA.Instruction (coerce opc) (FC.fmapFC (\(DA.Annotated a o) -> DA.Annotated (f a) o) operands))
      ARMBytes bs -> ARMBytes bs
      ThumbInstruction (DT.Instruction opc operands) ->
        ThumbInstruction (DT.Instruction (coerce opc) (FC.fmapFC (\(DT.Annotated a o) -> DT.Annotated (f a) o) operands))
-}

instance Eq (Instruction tp ()) where
  ARMInstruction i1 == ARMInstruction i2 = i1 == i2
  ARMBytes bs1 == ARMBytes bs2 = bs1 == bs2
  ARMInstruction {} == ARMBytes {} = False
  ARMBytes {} == ARMInstruction {} = False
  ThumbInstruction i1 == ThumbInstruction i2 = i1 == i2

instance Eq (Operand tp) where
  ARMOperand o1 == ARMOperand o2 = isJust (PC.testEquality o1 o2)
  ThumbOperand o1 == ThumbOperand o2 = isJust (PC.testEquality o1 o2)

instance Ord (Operand tp) where
  compare (ARMOperand o1) (ARMOperand o2) = PC.toOrdering (PC.compareF o1 o2)
  compare (ThumbOperand o1) (ThumbOperand o2) = PC.toOrdering (PC.compareF o1 o2)

instance PC.TestEquality Operand where
  testEquality (ARMOperand o1) (ARMOperand o2) = do
    PC.Refl <- PC.testEquality o1 o2
    return PC.Refl
  testEquality (ThumbOperand o1) (ThumbOperand o2) = do
    PC.Refl <- PC.testEquality o1 o2
    return PC.Refl
  testEquality _ _ = Nothing

instance PC.OrdF Operand where
  compareF (ARMOperand o1) (ARMOperand o2) =
    case PC.compareF o1 o2 of
      PC.EQF -> PC.EQF
      PC.LTF -> PC.LTF
      PC.GTF -> PC.GTF
  compareF (ThumbOperand o1) (ThumbOperand o2) =
    case PC.compareF o1 o2 of
      PC.EQF -> PC.EQF
      PC.LTF -> PC.LTF
      PC.GTF -> PC.GTF
  compareF (ARMOperand _) _ = PC.GTF
  compareF (ThumbOperand _) _ = PC.LTF

data InstructionDisassemblyFailure =
  InstructionDisassemblyFailure LBS.ByteString Int
  | EmptyBlock (R.ConcreteAddress MA.ARM)
  | OverlappingBlocks (R.ConcreteAddress MA.ARM) (R.ConcreteAddress MA.ARM)
  deriving (Show)

instance C.Exception InstructionDisassemblyFailure

assemble :: (C.MonadThrow m) => Instruction tp () -> m BS.ByteString
assemble i =
  case i of
    ARMInstruction ai -> return (LBS.toStrict (DA.assembleInstruction (armDropAnnotations ai)))
    ARMBytes bs -> return bs
    ThumbInstruction ti -> return (LBS.toStrict (DT.assembleInstruction (thumbDropAnnotations ti)))

data MySuperGreatException = MySuperGreatException String
  deriving (Show)

instance C.Exception MySuperGreatException

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
            => MD.ParsedBlock MA.ARM ids
            -> R.ConcreteAddress MA.ARM
            -> R.ConcreteAddress MA.ARM
            -> BS.ByteString
            -> m (R.ConcreteBlock MA.ARM)
disassemble pb startAddr endAddr bs0 = do
  let acon = toAnnotatedARM A32Repr
  let minsns0 = go A32Repr acon DA.disassembleInstruction 0 startAddr (LBS.fromStrict bs0) []
  -- case DLN.nonEmpty =<< minsns0 of
  case minsns0 of
    Right insns' | Just insns <- DLN.nonEmpty insns' -> return (R.concreteBlock startAddr insns (A32Repr) pb)
                 | otherwise -> C.throwM $ MySuperGreatException (show insns')
    Left e -> do
      let tcon = ThumbInstruction . toAnnotatedThumb
      let minsns1 = go T32Repr tcon DT.disassembleInstruction 0 startAddr (LBS.fromStrict bs0) []
      case minsns1 of
        Right insns' | Just insns <- DLN.nonEmpty insns' -> return (R.concreteBlock startAddr insns (T32Repr) pb)
                     | otherwise -> C.throwM $ MySuperGreatException (show e ++ "Examine code to figure out what this means: " ++ show insns')
        Left e' -> C.throwM $ MySuperGreatException ("Examine code to figure out what this means, part 2: " ++ show e)
  where
    go :: forall tp i m'
        . (C.MonadThrow m')
       => ARMRepr tp
       -> (i -> Instruction tp ())
       -> (LBS.ByteString -> (Int, Maybe i))
       -> Int
       -> R.ConcreteAddress MA.ARM
       -> LBS.ByteString
       -> [Instruction tp ()]
       -> m' [Instruction tp ()]
    go repr con dis totalRead insnAddr b insns =
      case dis b of
        (bytesRead, Nothing) ->
          C.throwM (InstructionDisassemblyFailure b bytesRead)
        (bytesRead, Just i) ->
          let nextAddr = insnAddr `R.addressAddOffset` fromIntegral bytesRead
          in if | nextAddr > endAddr -> trace ("\n\n ** bytesRead: " ++ show bytesRead ++ "\n ** nextAddr: " ++ show nextAddr ++ "\n ** insnAddr: " ++ show insnAddr ++ "\n ** startAddr: " ++ show startAddr ++ "\n ** endAddr: " ++ show endAddr) $ C.throwM (OverlappingBlocks startAddr endAddr)
                | nextAddr == endAddr ->
                  return (reverse (con i : insns))
                | otherwise ->
                  let b' = LBS.drop (fromIntegral bytesRead) b
                  in go repr con dis (totalRead + bytesRead) nextAddr b' (con i : insns)

armPrettyInstruction :: Instruction tp a -> String
armPrettyInstruction i =
  case i of
    ARMInstruction ai -> show (DA.ppInstruction (armDropAnnotations ai))
    ARMBytes bs -> ".word " ++ show bs
    ThumbInstruction ti -> show (DT.ppInstruction (thumbDropAnnotations ti))

armDropAnnotations :: DA.AnnotatedInstruction a -> DA.Instruction
armDropAnnotations i =
  case i of
    DA.Instruction opc annotatedOps ->
      DA.Instruction (coerce opc) (FC.fmapFC armUnannotateOpcode annotatedOps)

toAnnotatedARM :: ARMRepr A32 -> DA.Instruction -> Instruction A32 ()
toAnnotatedARM _ i =
  case i of
    DA.Instruction opc ops ->
      ARMInstructionBare (DA.Instruction (coerce opc) (FC.fmapFC (DA.Annotated ()) ops))

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
    ARMBytes bs -> fromIntegral (BS.length bs)
    ThumbInstruction ti ->
      let bytes = DT.assembleInstruction (thumbDropAnnotations ti)
      in fromIntegral (LBS.length bytes)

armMakePadding :: Word64 -> R.InstructionArchRepr MA.ARM tp -> [Instruction tp ()]
armMakePadding nBytes repr =
  case repr of
    A32Repr
      | leftoverARM == 0 ->
        fmap (toAnnotatedARM repr) (replicate (fromIntegral nARMInsns) aBrk)
      | otherwise ->
        RP.panic RP.ARMISA "armMakePadding" [ "Unexpected byte count (A32): " ++ show nBytes
                                            , "Only instruction-sized padding (4 bytes) is supported"
                                            ]
    T32Repr
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

armMakeRelativeJumpTo :: R.ConcreteAddress MA.ARM
                      -> R.ConcreteAddress MA.ARM
                      -> R.InstructionArchRepr MA.ARM tp
                      -> DLN.NonEmpty (Instruction tp ())
armMakeRelativeJumpTo src dest repr =
  case repr of
    A32Repr ->
      let off24 = jumpOffset src dest
      in singleton (DA.Instruction DA.B_A1 (DA.Bv4 unconditional DA.:< DA.Bv24 off24 DA.:< DA.Nil))
    T32Repr ->
      RP.panic RP.ARMISA "armMakeRelativeJumpTo" [ "Thumb rewriting is not yet supported"
                                                 ]

-- We have to correct by 8 because reading the PC in ARM actually
-- returns PC+8 due to some odd interactions with prefetch and the ISA
-- definition.
--
-- We further have to shift by two because branch offsets are stored
-- this way to ensure alignment (and compactness)
--
-- NOTE: Our offsets are signed, but the operands to ARM instructions are
-- all unsigned fixed-width words.  We need to do a safe conversion to an
-- unsigned value (keeping the same bit pattern) into the W type.
jumpOffset :: (MM.MemWidth (MC.ArchAddrWidth arch))
           => R.ConcreteAddress arch
           -> R.ConcreteAddress arch
           -> W.W 24
jumpOffset source target =
  W.wRep (PN.knownNat @24) adjustedOffset
  where
    -- Offset we actually want
    rawOff :: Integer
    rawOff = toInteger (target `R.addressDiff` source)
    -- Offset adjusted to account for the PC diff (by 8) and the required right
    -- shift for alignment
    adjustedOffset :: Integer
    adjustedOffset = (rawOff - 8) `DB.shiftR` 2


unconditional :: W.W 4
unconditional = 14

-- | The maximum range we can jump on 32 bit ARM
--
-- Note that the branching instructions have limited range: 14 bits on A32, 10
-- on T32.  This is not enough for any reasonable code relocation.  However, we
-- have worked around this with a code sequence that embeds the jump offset into
-- the instruction stream, allowing us to load 4 byte offsets (at the cost of an
-- extra dereference).  This gets us a full 32 bit range.
--
-- NOTE: We need to update the T32 range when we implement thumb support
armMaxRelativeJumpSize :: R.InstructionArchRepr MA.ARM tp -> Word64
armMaxRelativeJumpSize repr =
  case repr of
    A32Repr -> DB.bit 30 - 4
    T32Repr -> DB.bit 10 - 4

asInteger :: forall n . (KnownNat n, 1 PN.<= n) => W.W n -> Integer
asInteger w = PN.toSigned (PN.knownNat @n) (toInteger w)

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
            -> R.ConcreteAddress MA.ARM
            -> MD.ParsedBlock MA.ARM ids
            -> Some (R.JumpType MA.ARM)
armJumpType i mem insnAddr pb =
  case asParsedTerminator mem insnAddr pb of
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
            AI (DA.Instruction DA.BL_i_A1 (DA.Bv4 _cond DA.:< DA.Bv24 (asInteger -> off) DA.:< DA.Nil)) ->
              Some (R.DirectCall insnAddr (fromIntegral (off `DB.shiftL` 2) + 8))
            AI (DA.Instruction DA.BL_i_A2 (DA.Bv1 _ DA.:< DA.Bv4 _ DA.:< DA.Bv24 (asInteger -> off) DA.:< DA.Nil)) ->
              Some (R.DirectCall insnAddr (fromIntegral (off `DB.shiftL` 2) + 8))
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
                 AI (DA.Instruction DA.B_A1 (DA.Bv4 _cond DA.:< DA.Bv24 (asInteger -> off) DA.:< DA.Nil)) ->
                   Some (R.RelativeJump R.Unconditional insnAddr (fromIntegral (off `DB.shiftL` 2) + 8))
                 -- FIXME: Handle Thumb cases
                 _ -> Some (R.NotInstrumentable insnAddr)
        -- Any instruction in ARM can be a conditional jump due to predication...
        --
        -- We are only handling B with condition codes here.
        MD.ParsedBranch {} ->
          case i of
            AI (DA.Instruction DA.B_A1 (DA.Bv4 _cond DA.:< DA.Bv24 (asInteger -> off) DA.:< DA.Nil)) ->
              Some (R.RelativeJump R.Conditional insnAddr (fromIntegral (off `DB.shiftL` 2) + 8))
            -- FIXME: Handle T32 cases
            _ -> Some (R.NotInstrumentable insnAddr)
        MD.ParsedLookupTable _regs _cond _tgts _ ->
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

armIsRelocatableJump :: Instruction tp (R.Relocation MA.ARM) -> Bool
armIsRelocatableJump i = case i of
  ARMInstructionAnn _ b -> b
  _ -> False

data CurrentInstruction = NoInstruction
                        | InInstruction (MC.MemSegmentOff 32)
                        | FoundTarget

-- | If the given address corresponds to the block terminator, return that
-- terminator.
--
-- This can panic if there is a major inconsistency
--
-- Scan through an open an active instruction at the 'MD.InstructionStart'
-- statement and close it at 'MD.ArchState'
asParsedTerminator :: MM.Memory 32
                   -> R.ConcreteAddress MA.ARM
                   -> MD.ParsedBlock MA.ARM ids
                   -> Maybe (MD.ParsedTermStmt MA.ARM ids)
asParsedTerminator mem insnAddr pb =
  case F.foldl' searchTarget NoInstruction (markLast (MD.pblockStmts pb)) of
    NoInstruction -> Nothing
    InInstruction addr
      | Just insnAddr == R.concreteFromSegmentOff mem addr -> Just (MD.pblockTermStmt pb)
      | otherwise -> Nothing
    FoundTarget ->
      -- If we found a 'MD.ArchState' corresponding to our target, it is a
      -- non-terminator instruction
      Nothing
  where
    searchTarget curState (stmt, isLast) =
      case curState of
        FoundTarget -> FoundTarget
        _ ->
          case stmt of
            MC.InstructionStart blockOff _ ->
              case MM.incSegmentOff (MD.pblockAddr pb) (fromIntegral blockOff) of
                Nothing -> NoInstruction
                Just insnAddrSegOff -> InInstruction insnAddrSegOff
            MC.ArchState addr _
              | isLast -> curState
              | Just caddrWord <- MM.asAbsoluteAddr addr
              , insnAddr == R.concreteFromAbsolute caddrWord -> FoundTarget
              | otherwise -> NoInstruction
            _ -> curState

-- | Mark the last element of a list with a Bool (True) indicating it is the last
markLast :: [a] -> [(a, Bool)]
markLast lst =
  case reverse (fmap (, False) lst) of
    [] -> []
    (lastItem, _) : rest -> reverse ((lastItem, True) : rest)

singleton :: DA.Instruction -> DLN.NonEmpty (Instruction A32 ())
singleton = (DLN.:| []) . toAnnotatedARM A32Repr

-- | Resolve relocations in instructions (turning them from symbolic instructions to concrete instructions)
--
-- On AArch32, we only use relocations for two things (right now):
--
-- * Jump offsets in the branch instructions
--
-- * Data references in load instructions
--
-- See Note [ARM Relocations] for some additional details and caveats
armConcretizeAddresses :: MM.Memory 32
                       -> (R.SymbolicAddress MA.ARM -> R.ConcreteAddress MA.ARM)
                       -> R.ConcreteAddress MA.ARM
                       -> Instruction tp (R.Relocation MA.ARM)
                       -> DLN.NonEmpty (Instruction tp ())
armConcretizeAddresses _mem toConcrete insnAddr i0 =
  case i0 of
    ARMBytes {} ->
      RP.panic RP.ARMISA "armConcretizeAddresses" [ "Illegal raw bytes at: " ++ show insnAddr ]
    ARMInstruction (DA.Instruction opc operands) ->
      case (opc, operands) of
        (DA.LDR_l_A1, DA.Annotated _ p
                DA.:< DA.Annotated _ rt
                DA.:< DA.Annotated _ u
                DA.:< DA.Annotated _ w
                DA.:< DA.Annotated _ cond
                DA.:< DA.Annotated (R.PCRelativeRelocation absAddr) _off12
                DA.:< DA.Nil) ->
          -- See Note [Rewriting LDR] for details on this construction
          let w32 = fromIntegral (R.absoluteAddress (absAddr `R.addressAddOffset` 8))
              i1 = ARMInstructionBare $ DA.Instruction DA.LDR_l_A1 (DA.Annotated () p DA.:< DA.Annotated () rt DA.:< DA.Annotated () u DA.:< DA.Annotated () w DA.:< DA.Annotated () cond DA.:< DA.Annotated () (DA.Bv12 0) DA.:< DA.Nil)
              i2 = toAnnotatedARM A32Repr $ DA.Instruction DA.B_A1 (DA.Bv4 unconditional DA.:< DA.Bv24 (0 `DB.shiftR` 2) DA.:< DA.Nil)
              i3 = ARMBytes (LBS.toStrict (BB.toLazyByteString (BB.word32LE w32)))
              -- Operands marked with ??? were blindly chosen by disassembling a similar instruction.
              i4 = ARMInstructionBare $ DA.Instruction DA.LDR_i_A1_off $
                     (DA.Annotated () (DA.Bv1 1) -- ???
                      DA.:< DA.Annotated () rt -- target register?
                      DA.:< DA.Annotated () rt -- source register?
                      DA.:< DA.Annotated () (DA.Bv1 1) -- ???
                      DA.:< DA.Annotated () (DA.Bv1 0) -- ???
                      DA.:< DA.Annotated () (DA.Bv4 14) -- ???
                      DA.:< DA.Annotated () (DA.Bv12 0) -- offset
                      DA.:< DA.Nil)
          in i1 DLN.:| [ i2, i3, i4 ]
        (DA.B_A1, DA.Annotated _ (DA.Bv4 cond)
            DA.:< DA.Annotated (R.SymbolicRelocation symTarget) (DA.Bv24 _off)
            DA.:< DA.Nil) ->
          let newOff = jumpOffset insnAddr (toConcrete symTarget)
          in singleton (DA.Instruction DA.B_A1 (DA.Bv4 cond DA.:< DA.Bv24 newOff DA.:< DA.Nil))
        (DA.BL_i_A1, DA.Annotated _ (DA.Bv4 cond)
               DA.:< DA.Annotated (R.SymbolicRelocation symTarget) (DA.Bv24 _off)
               DA.:< DA.Nil) ->
          let newOff = jumpOffset insnAddr (toConcrete symTarget)
          in singleton (DA.Instruction DA.BL_i_A1 (DA.Bv4 cond DA.:< DA.Bv24 newOff DA.:< DA.Nil))
        (DA.BL_i_A2, DA.Annotated _ (DA.Bv1 b1)
               DA.:< DA.Annotated _ (DA.Bv4 cond)
               DA.:< DA.Annotated (R.SymbolicRelocation symTarget) (DA.Bv24 _off)
               DA.:< DA.Nil) ->
          let newOff = jumpOffset insnAddr (toConcrete symTarget)
          in singleton (DA.Instruction DA.BL_i_A2 (DA.Bv1 b1 DA.:< DA.Bv4 cond DA.:< DA.Bv24 newOff DA.:< DA.Nil))
        _ -> ARMInstructionBare (DA.Instruction (coerce opc) (FC.fmapFC toUnitAnnotation operands)) DLN.:| []
    ThumbInstruction {} ->
      RP.panic RP.ARMISA "armConcretizeAddresses" [ "Thumb rewriting is not yet supported"
                                                  ]
  where
    toUnitAnnotation :: forall tp
                      . DA.Annotated (R.Relocation MA.ARM) DA.Operand tp
                     -> DA.Annotated () DA.Operand tp
    toUnitAnnotation (DA.Annotated _ op) = DA.Annotated () op

-- | Compute the type of relocation to generate for the offset of the given jump instruction (if any)
--
-- This really has meaning for absolute (rare) and relative jumps.  Calls and
-- indirect jumps do not get relocations in renovate.
jumpTargetRelocation
  :: MM.Memory 32
  -> (R.ConcreteAddress MA.ARM -> R.SymbolicAddress MA.ARM)
  -> MD.ParsedBlock MA.ARM ids
  -> R.ConcreteAddress MA.ARM
  -> Instruction tp ()
  -> Maybe (R.Relocation MA.ARM)
jumpTargetRelocation mem toSymbolic pb insnAddr i =
  case armJumpType i mem insnAddr pb of
    Some (R.RelativeJump _jc src offset) -> Just (R.SymbolicRelocation (toSymbolic (src `R.addressAddOffset` offset)))
    Some (R.AbsoluteJump _jc target) -> Just (R.SymbolicRelocation (toSymbolic target))
    Some (R.IndirectJump {}) -> Nothing
    Some (R.DirectCall src offset) -> Just (R.SymbolicRelocation (toSymbolic (src `R.addressAddOffset` offset)))
    Some R.IndirectCall -> Nothing
    Some (R.Return {}) -> Nothing
    Some R.NoJump -> Nothing
    Some (R.NotInstrumentable _) -> Nothing

noRelocation :: DA.Operand tp -> DA.Annotated (R.Relocation MA.ARM) DA.Operand tp
noRelocation = DA.Annotated R.NoRelocation

armInstruction
  :: MM.Memory 32
  -> MD.ParsedBlock MA.ARM ids
  -> R.ConcreteAddress MA.ARM
  -> DA.AnnotatedInstruction (R.Relocation MA.ARM)
  -> Instruction A32 (R.Relocation MA.ARM)
armInstruction mem pb insnAddr anni = 
  let jt = armJumpType (toAnnotatedARM A32Repr (armDropAnnotations anni)) mem insnAddr pb
  in ARMInstructionAnn anni (R.isRelocatableJump jt)

armSymbolizeAddresses :: MM.Memory 32
                      -> (R.ConcreteAddress MA.ARM -> R.SymbolicAddress MA.ARM)
                      -> MD.ParsedBlock MA.ARM ids
                      -> R.ConcreteAddress MA.ARM
                      -> Instruction tp ()
                      -> [R.Instruction MA.ARM tp (R.Relocation MA.ARM)]
armSymbolizeAddresses mem toSymbolic pb insnAddr i =
--  trace (show pb) $
  case i of
    ARMInstruction (DA.Instruction opc operands) ->
      case (opc, operands) of
        (DA.LDR_l_A1, DA.Annotated _ p
                DA.:< DA.Annotated _ rt
                DA.:< DA.Annotated _ u
                DA.:< DA.Annotated _ w
                DA.:< DA.Annotated _ cond
                DA.:< DA.Annotated _ (DA.Bv12 (asInteger -> off12))
                DA.:< DA.Nil) ->
          -- See Note [Rewriting LDR] for details on this construction
          let target = insnAddr `R.addressAddOffset` fromIntegral off12
              i' = DA.Instruction (coerce opc) (     noRelocation p
                                               DA.:< noRelocation rt
                                               DA.:< noRelocation u
                                               DA.:< noRelocation w
                                               DA.:< noRelocation cond
                                               DA.:< DA.Annotated (R.PCRelativeRelocation target) (DA.Bv12 0)
                                               DA.:< DA.Nil
                                               )
          in [armInstruction mem pb insnAddr i']
        (DA.B_A1, DA.Annotated _ cond@(DA.Bv4 _) DA.:< DA.Annotated _ (DA.Bv24 _off) DA.:< DA.Nil)
          | Just reloc <- jumpTargetRelocation mem toSymbolic pb insnAddr i ->
              let i' = DA.Instruction (coerce opc) (     noRelocation cond
                                                   DA.:< DA.Annotated reloc (DA.Bv24 0)
                                                   DA.:< DA.Nil
                                                   )
              in [armInstruction mem pb insnAddr i']
        (DA.BL_i_A1, DA.Annotated _ cond@(DA.Bv4 _) DA.:< DA.Annotated _ (DA.Bv24 _off) DA.:< DA.Nil)
          | Just reloc <- jumpTargetRelocation mem toSymbolic pb insnAddr i ->
            let i' = DA.Instruction (coerce opc) (     noRelocation cond
                                                 DA.:< DA.Annotated reloc (DA.Bv24 0)
                                                 DA.:< DA.Nil
                                                 )
            in [armInstruction mem pb insnAddr i']
        (DA.BL_i_A2, DA.Annotated _ b1@(DA.Bv1 _) DA.:< DA.Annotated _ cond@(DA.Bv4 _) DA.:< DA.Annotated _ (DA.Bv24 _off) DA.:< DA.Nil)
          | Just reloc <- jumpTargetRelocation mem toSymbolic pb insnAddr i ->
            let i' = DA.Instruction (coerce opc) (     noRelocation b1
                                                 DA.:< noRelocation cond
                                                 DA.:< DA.Annotated reloc (DA.Bv24 0)
                                                 DA.:< DA.Nil
                                                 )
            in [armInstruction mem pb insnAddr i']
        _ ->
          let i' = DA.Instruction (coerce opc) (FC.fmapFC (\(DA.Annotated _ c) -> DA.Annotated R.NoRelocation c) operands)
          in [armInstruction mem pb insnAddr i']
    ARMBytes {} ->
      RP.panic RP.ARMISA "armSymbolizeAddresses" [ "Raw bytes are not allowed in the instruction stream during symbolization at: " ++ show insnAddr ]
    ThumbInstruction {} ->
      RP.panic RP.ARMISA "armSymbolizeAddresses" [ "Thumb rewriting is not yet support"
                                                 , show pb ]

isa :: R.ISA MA.ARM
isa =
  R.ISA { R.isaInstructionSize = armInstrSize
        , R.isaPrettyInstruction = armPrettyInstruction
        , R.isaMakePadding = armMakePadding
        , R.isaInstructionRepr = armInstructionRepr
        , R.isaInstructionArchReprs =
          R.SomeInstructionArchRepr A32Repr DLN.:|
          [R.SomeInstructionArchRepr T32Repr]
        , R.isaMakeRelativeJumpTo = armMakeRelativeJumpTo
        , R.isaMaxRelativeJumpSize = armMaxRelativeJumpSize
        , R.isaJumpType = armJumpType
        , R.isaIsRelocatableJump = armIsRelocatableJump
        , R.isaConcretizeAddresses = armConcretizeAddresses
        , R.isaSymbolizeAddresses = armSymbolizeAddresses
        }

{- Note [ARM Relocations]

Support for ARM is not complete yet, but is sufficient for many normal
compiler-generated binaries.

We currently only properly use relocations for two types of operand:

- Jump offsets in the branch instructions
- Data references in load instructions

* Jumps

This approach does not handle jumps performed by directly modifying the PC. This
mode is deprecated, but can still show up.  We mark those jumps as
un-instrumentable for now.

* Data References

Note that we do not handle store instructions explicitly. This is because the
common form of memory reference in ARM is of the form:

#+BEGIN_SRC
ldr rt, [pc, #NN]
ldr rt, [rt, #0]
#+END_SRC

or

#+BEGIN_SRC
ldr rt, [pc, #NN]
str r1, [rt, #0]
#+END_SRC

That is: references are through indirection tables stored near the
instruction. This is because the "reach" of each load/store instruction is very
limited due to the limited bits allocated to the offset.  Storing addresses in
indirection tables near the instruction allows it to load arbitrary
addresses. Note that for the store case, we only need to update the accompanying
~ldr~ in order to fix any changes due to relocation, as the loaded address is
absolute.

This has some consequences:
- In a small memory codegen mode, indirection tables might not be used and this
  strategy is incomplete
- There could be some variants of this pattern that different compilers use that
  we will need to look out for
- This pattern will not cover Position Independent Code, where the indirection
  tables contain *offsets from the PC*; handling these will likely be somewhat
  complicated

-}

{- Note [Rewriting LDR]

As noted in Note [ARM Relocations], we have to rewrite some LDR forms when we
move instructions.

PC-relative data references via LDR (most seem to be LDR related)

Problem: LDR only has 12 bits of offset, so only data within 12
bits (4kb) of the instruction address can be referenced.  This
isn't really enough to let us reference data from the original
text section from the new one.

We will fix that by translating into a three instruction sequence:

> ldr rt, [pc, #8]
> ldr [rt], [rt, #0]
> b +8
> .word <address that the data is really at>

That is: we put the real address of the original piece of data
inline as an absolute address that we can load locally.  We add a
fallthrough jump to make sure that we never execute the inline
data.

This is a bit awkward, as the address we are loading is actually
a pointer to a table that has the address of the real variable
are accessing in many cases.  However, it isn't guaranteed to be
that.

Note that the pseudo-code above is reflected oddly below.

 * First, the computed address has to be offset by 8 to account
   for the odd semantics of reading the PC.
 * Second, the two jump offsets are 0 for the same reason (you
   automatically get a +8 vs the PC, so we don't need any extra)

-}
