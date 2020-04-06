{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renovate.Arch.PPC.ISA (
  isa,
  assemble,
  disassemble,
  Instruction,
  TargetAddress(..),
  OnlyEncoding,
  fromInst,
  toInst,
  Operand(..),
  PPCRepr(..),
  -- * Exceptions
  InstructionDisassemblyFailure(..)
  ) where

import           GHC.Stack ( HasCallStack )

import qualified Control.Monad.Catch as C
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Coerce ( coerce )
import           Data.Int ( Int32 )
import qualified Data.List.NonEmpty as DLN
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Typeable ( Typeable )
import           Data.Word ( Word8, Word64 )
import           Text.Printf ( printf )

import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.PPC as MP
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Dismantle.PPC as D

import qualified Renovate as R
import qualified Renovate.Arch.PPC.Panic as RP

data TargetAddress arch = NoAddress
                        deriving (Eq, Ord)

deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (TargetAddress arch)

newtype Instruction tp a = I { unI :: D.AnnotatedInstruction a }
  deriving (Eq, Show)

data EncodingKind = OnlyEncoding
  deriving (Typeable)
type OnlyEncoding = 'OnlyEncoding

data PPCRepr tp where
  PPCRepr :: PPCRepr OnlyEncoding

instance TestEquality PPCRepr where
  testEquality PPCRepr PPCRepr = Just Refl

instance OrdF PPCRepr where
  compareF PPCRepr PPCRepr = EQF

type instance R.InstructionArchReprKind (MP.AnyPPC v) = EncodingKind
type instance R.InstructionArchRepr (MP.AnyPPC v) = PPCRepr

data Operand (tp :: EncodingKind) where
  Operand :: D.Operand sh -> Operand OnlyEncoding

instance TestEquality Operand where
  testEquality (Operand o1) (Operand o2) = do
    Refl <- testEquality o1 o2
    return Refl

instance OrdF Operand where
  compareF (Operand o1) (Operand o2) =
    case compareF o1 o2 of
      EQF -> EQF
      LTF -> LTF
      GTF -> GTF

instance Eq (Operand tp) where
  o1 == o2 = isJust (testEquality o1 o2)

instance Ord (Operand tp) where
  compare o1 o2 = toOrdering (compareF o1 o2)

type instance R.Instruction (MP.AnyPPC v) = Instruction
type instance R.InstructionAnnotation (MP.AnyPPC v) = TargetAddress (MP.AnyPPC v)
type instance R.RegisterType (MP.AnyPPC v) = Operand

instance PP.Pretty (Instruction tp a) where
  pretty = PP.pretty . ppcPrettyInstruction

instance Functor (Instruction tp) where
  fmap f (I i) =
    case i of
      D.Instruction opc operands ->
        I (D.Instruction (coerce opc) (FC.fmapFC (\(D.Annotated a o) -> D.Annotated (f a) o) operands))

assemble :: (C.MonadThrow m) => Instruction tp () -> m B.ByteString
assemble = return . LB.toStrict . D.assembleInstruction . toInst

disassemble :: (C.MonadThrow m, MM.MemWidth (MM.ArchAddrWidth (MP.AnyPPC v)))
            => MD.ParsedBlock (MP.AnyPPC v) ids
            -> R.ConcreteAddress (MP.AnyPPC v)
            -> R.ConcreteAddress (MP.AnyPPC v)
            -> B.ByteString
            -> m (R.ConcreteBlock (MP.AnyPPC v))
disassemble pb start end b0 = do
  insns0 <- go 0 start b0 []
  case DLN.nonEmpty insns0 of
    Nothing -> C.throwM (EmptyBlock (show start))
    Just insns -> return (R.concreteBlock start insns PPCRepr pb)
  where
    go totalRead insnAddr b insns =
      case D.disassembleInstruction (LB.fromStrict b) of
        (bytesRead, Nothing) ->
          C.throwM (InstructionDisassemblyFailure b bytesRead)
        (bytesRead, Just i) ->
          let nextAddr = insnAddr `R.addressAddOffset` fromIntegral bytesRead
          in if | nextAddr > end -> C.throwM (OverlappingBlocks b)
                | nextAddr == end ->
                  return (reverse (fromInst i : insns))
                | otherwise ->
                  go (totalRead + bytesRead) nextAddr (B.drop bytesRead b) (fromInst i : insns)

data InstructionDisassemblyFailure =
  InstructionDisassemblyFailure B.ByteString Int
  | EmptyBlock String
  | OverlappingBlocks B.ByteString
  deriving (Typeable, Show)

instance C.Exception InstructionDisassemblyFailure

-- | An 'ISA' description for PowerPC
--
-- For now, the same description works for both PowerPC 32 and PowerPC 64.
isa :: ( arch ~ MP.AnyPPC v
       , MM.MemWidth (MM.ArchAddrWidth arch)
       , R.Instruction arch ~ Instruction
       , R.InstructionAnnotation arch ~ TargetAddress arch)
    => R.ISA arch
isa =
  R.ISA { R.isaInstructionSize = ppcInstrSize
        , R.isaPrettyInstruction = ppcPrettyInstruction
        , R.isaMakePadding = ppcMakePadding
        , R.isaInstructionArchReprs = R.SomeInstructionArchRepr PPCRepr DLN.:| []
        , R.isaMakeRelativeJumpTo = ppcMakeRelativeJumpTo
        , R.isaMaxRelativeJumpSize = const ppcMaxRelativeJumpSize
        , R.isaJumpType = ppcJumpType
        , R.isaModifyJumpTarget = ppcModifyJumpTarget
        , R.isaMakeSymbolicJump = ppcMakeSymbolicJump
        , R.isaConcretizeAddresses = ppcConcretizeAddresses
        , R.isaSymbolizeAddresses = ppcSymbolizeAddresses
        }

ppcPrettyInstruction :: Instruction tp a -> String
ppcPrettyInstruction = show . D.ppInstruction . toInst

-- | All instructions on PowerPC are 4 bytes
ppcInstrSize :: Instruction tp a -> Word8
ppcInstrSize _ = 4

-- | Make the requested number of bytes of padding instructions (as TRAP
-- instructions).  We can only support numbers of bytes that are multiples of
-- four, as that is the only instruction size on PowerPC.
ppcMakePadding :: (HasCallStack)
               => Word64
               -> R.InstructionArchRepr (MP.AnyPPC v) tp
               -> [Instruction tp ()]
ppcMakePadding nBytes _
  | leftover == 0 = replicate nInsns (fromInst nopInsn)
  | otherwise = error (printf "Unexpected byte count (%d); PowerPC only supports instruction-sized padding" nBytes)
  where
    (nInsns, leftover) = fromIntegral nBytes `divMod` 4
    nopInsn = D.Instruction D.ATTN D.Nil

-- | Make an unconditional relative jump from the given @srcAddr@ to the
-- @targetAddr@.
ppcMakeRelativeJumpTo :: (MM.MemWidth (MM.ArchAddrWidth arch))
                      => R.ConcreteAddress arch
                      -> R.ConcreteAddress arch
                      -> R.InstructionArchRepr arch tp
                      -> DLN.NonEmpty (Instruction tp ())
ppcMakeRelativeJumpTo srcAddr targetAddr _
  | offset `mod` 4 /= 0 =
    error (printf "Unaligned jump with source=%s and target=%s" (show srcAddr) (show targetAddr))
  | offset > fromIntegral ppcMaxRelativeJumpSize =
    error (printf "Jump target is too far away with source=%s and target=%s" (show srcAddr) (show targetAddr))
  | otherwise = fromInst jumpInstr DLN.:| []
  where
    -- We are limited to 24 + 2 bits of offset, where the low two bits must be zero.
    offset :: Integer
    offset = targetAddr `R.addressDiff` srcAddr

    -- We checked to make sure the low bits are zero with the mod case above.
    -- Now we shift off two of the required zeros.
    shiftedOffset :: Int32
    shiftedOffset = fromIntegral offset `shiftR` 2
    jumpInstr = D.Instruction D.B (D.Directbrtarget (D.BT shiftedOffset) D.:< D.Nil)

-- There are 24 bits of offset in the branch instruction, plus 2 zero bits are
-- added for you at the end -- but it's signed, so lose one bit for that for a
-- total of 25 bits in either direction. We subtract 4 instead of 1 also
-- because of the two zero bits implicitly added to each jump offset.
ppcMaxRelativeJumpSize :: Word64
ppcMaxRelativeJumpSize = bit 25 - 4

ppcMakeSymbolicJump :: (MM.MemWidth (MM.ArchAddrWidth arch), R.Instruction arch ~ Instruction)
                    => R.SymbolicAddress arch
                    -> R.InstructionArchRepr arch tp
                    -> [R.TaggedInstruction arch tp (TargetAddress arch)]
ppcMakeSymbolicJump symAddr _ = [R.tagInstruction (Just symAddr) i]
  where
    -- The jump has an invalid destination because it is just a stand-in; it
    -- will be rewritten with a real jump target when we concretize the
    -- instruction.
    jmp = D.Instruction D.B (D.Directbrtarget (D.BT 0) D.:< D.Nil)
    i = annotateInstr (fromInst jmp) NoAddress

-- | This function converts symbolic address references in operands back to
-- concrete values.  As with 'ppcSymbolizeAddresses', it is a no-op on PowerPC.
ppcConcretizeAddresses :: (MM.MemWidth (MM.ArchAddrWidth arch))
                       => MM.Memory (MM.ArchAddrWidth arch)
                       -> R.ConcreteAddress arch
                       -> Instruction tp (TargetAddress arch)
                       -> Instruction tp ()
ppcConcretizeAddresses _mem _addr i =
  case unI i of
    D.Instruction opc operands ->
      I (D.Instruction (coerce opc) (FC.fmapFC (\(D.Annotated _ operand) -> D.Annotated () operand) operands))

-- | This function records the real addresses of IP-relative addressing operands.
--
-- In PowerPC, there are no IP-relative addressing operands (besides jumps), so
-- this is a no-op.  Jumps are handled separately (via the 'TaggedInstruction'
-- wrapper).  Since the 'TaggedInstruction' doesn't need to modify the
-- instruction, it can actually be generated in an architecture-independent way
-- (i.e., not in an architecture-specific backend).
--
-- We rewrite all conditional branches into the sequence described in Note
-- [Conditional Branch Restrictions] to allow us flexibility in code layout.
-- Unconditional branches are left alone (though they are tagged with a symbolic
-- address, if requested).
--
-- Note that the long unconditional jump we add has a dummy target, as the real
-- target is specified through the symbolic target.
ppcSymbolizeAddresses :: (MM.MemWidth (MM.ArchAddrWidth arch), R.Instruction arch ~ Instruction)
                      => MM.Memory (MM.ArchAddrWidth arch)
                      -> R.ConcreteAddress arch
                      -> Maybe (R.SymbolicAddress arch)
                      -> Instruction tp ()
                      -> [R.TaggedInstruction arch tp (TargetAddress arch)]
ppcSymbolizeAddresses _mem _insnAddr mSymbolicTarget i = case unI i of
  D.Instruction opc operands ->
    let newInsn = D.Instruction (coerce opc) (FC.fmapFC annotateNull operands)
    in [R.tagInstruction mSymbolicTarget (I newInsn)]
  where
    annotateNull (D.Annotated _ operand) = D.Annotated NoAddress operand

-- | Classify jumps (and determine their targets, where possible)
ppcJumpType :: (HasCallStack, MM.MemWidth (MM.ArchAddrWidth arch))
            => Instruction tp t
            -> MM.Memory (MM.ArchAddrWidth arch)
            -> R.ConcreteAddress arch
            -> unused
            -> Some (R.JumpType arch)
ppcJumpType i _mem insnAddr _ =
  case toInst i of
    D.Instruction opc operands ->
      case operands of
        D.Calltarget (D.BT offset) D.:< D.Nil ->
          Some (R.DirectCall insnAddr (fromIntegral (offset `shiftL` 2)))
        D.Directbrtarget (D.BT offset) D.:< D.Nil ->
          Some (R.RelativeJump R.Unconditional insnAddr (fromIntegral (offset `shiftL` 2)))
        -- GBC has an extra argument generalizing to include a branch hint
        D.Condbrtarget (D.CBT offset) D.:< _crbit D.:< _bh D.:< D.Nil ->
          Some (R.RelativeJump R.Conditional insnAddr (fromIntegral (offset `shiftL` 2)))
        D.Condbrtarget (D.CBT offset) D.:< _crbit D.:< D.Nil ->
          Some (R.RelativeJump R.Conditional insnAddr (fromIntegral (offset `shiftL` 2)))
        D.Condbrtarget (D.CBT offset) D.:< D.Nil ->
          case opc of
            D.BCLalways ->
              Some (R.RelativeJump R.Unconditional insnAddr (fromIntegral (offset `shiftL` 2)))
            _ ->
              Some (R.RelativeJump R.Conditional insnAddr (fromIntegral (offset `shiftL` 2)))
        D.Absdirectbrtarget (D.ABT addr) D.:< D.Nil ->
          Some (R.AbsoluteJump R.Unconditional (R.concreteFromAbsolute (fromIntegral (addr `shiftL` 2))))
        D.Abscondbrtarget (D.ACBT addr) D.:< D.Nil ->
          Some (R.AbsoluteJump R.Conditional (R.concreteFromAbsolute (fromIntegral (addr `shiftL` 2))))
        D.Abscondbrtarget (D.ACBT addr) D.:< _ D.:< _ D.:< D.Nil ->
          Some (R.AbsoluteJump R.Conditional (R.concreteFromAbsolute (fromIntegral (addr `shiftL` 2))))
        D.Nil ->
          case opc of
            D.BCTR -> Some (R.IndirectJump R.Unconditional)
            D.BCTRL -> Some R.IndirectCall
            D.TRAP -> Some R.IndirectCall
            -- Conditional branches to link register
            D.BDNZLR -> Some R.IndirectCall    -- Some kind of conditional return
            D.BDNZLRL -> Some R.IndirectCall   -- Conditional return and link
            D.BDNZLRLm -> Some R.IndirectCall
            D.BDNZLRLp -> Some R.IndirectCall
            D.BDNZLRm -> Some R.IndirectCall
            D.BDNZLRp -> Some R.IndirectCall
            D.BDZLR -> Some R.IndirectCall
            D.BDZLRL -> Some R.IndirectCall
            D.BDZLRLm -> Some R.IndirectCall
            D.BDZLRLp -> Some R.IndirectCall
            D.BDZLRm -> Some R.IndirectCall
            D.BDZLRp -> Some R.IndirectCall
            -- Normal return (branch to link register)
            D.BLR -> Some (R.Return R.Unconditional)
            D.BLRL -> Some (R.Return R.Unconditional)
            _ -> Some R.NoJump
        (_ D.:< _) ->
          -- In this case, we handle all of the branches that don't need to inspect
          -- operands (because they are indirect)
          case opc of
            -- Conditional branch through the CTR register
            D.BCCTR -> Some (R.IndirectJump R.Conditional)
            D.GBCCTR -> Some (R.IndirectJump R.Conditional)
            -- This is a call because it is setting the link register and could
            -- return to the next instruction
            D.BCCTRL -> Some R.IndirectCall
            D.BCL -> Some R.IndirectCall
            D.GBCL -> Some R.IndirectCall
            D.GBCCTRL -> Some R.IndirectCall
            -- Syscall
            D.SC -> Some R.IndirectCall
            -- Traps
            D.TW -> Some R.IndirectCall
            D.TWI -> Some R.IndirectCall
            D.TD -> Some R.IndirectCall
            D.TDI -> Some R.IndirectCall
            -- Returns with extra operands
            D.GBCLR -> Some (R.Return R.Conditional)
            D.GBCLRL -> Some (R.Return R.Conditional)
            _ -> Some R.NoJump

-- | Given a jump instruction and a new target address, update the jump
-- instruction to target the new address.
--
-- This function also takes the address of the instruction so that it can
-- compute IP-relative jump offsets.
--
-- Note that we add two bits of available space when we compute the new jump
-- offsets; this is because there are two zero bits implicitly concatenated to
-- the right of the jump offset.  'newJumpOffset' checks the alignment
-- requirement and the range.  When we construct the operand, we shift off the
-- two zero bits.
--
-- See Note [Conditional Branch Restrictions]
ppcModifyJumpTarget :: (HasCallStack, MM.MemWidth (MM.ArchAddrWidth arch), R.Instruction arch ~ Instruction)
                    => R.ConcreteAddress arch
                    -- ^ The address of the instruction
                    -> R.Instruction arch tp ()
                    -- ^ The instruction to modify, with new targets attached
                    -> R.RelocatableTarget arch R.ConcreteAddress R.HasSomeTarget
                    -> Maybe (DLN.NonEmpty (Instruction tp ()))
ppcModifyJumpTarget srcAddr i (R.RelocatableTarget targetAddr) =
  case unI i of
    D.Instruction opc operands ->
      case operands of
        D.Annotated a (D.Calltarget (D.BT _offset)) D.:< D.Nil -> do
          off <- absoluteOff 0 targetAddr
          return (I (D.Instruction opc (D.Annotated a (D.Calltarget off) D.:< D.Nil)) DLN.:| [])
        D.Annotated a (D.Directbrtarget (D.BT _offset)) D.:< D.Nil -> do
          off <- absoluteOff 0 targetAddr
          return (I (D.Instruction opc (D.Annotated a (D.Directbrtarget off) D.:< D.Nil)) DLN.:| [])
        D.Annotated a (D.Condbrtarget (D.CBT _offset)) D.:< rest ->
          -- We add 8 to the "source" address because, in the case we use this
          -- computed address, the conditional branch actually comes after two
          -- more instructions (noops).  Thus, we correct the srcAddr by two
          -- instructions.
          case newJumpOffset 16 (srcAddr `R.addressAddOffset` 8) targetAddr of
            Right tgtOff4 -> do
              -- In this case, the jump target is within range of a 16 bit
              -- offset for a conditional branch. That means that we can simply
              -- update the target of the conditional branch directly.
              --
              -- NOTE: We have to add two no-ops here to keep the block size the
              -- same between both this good case and the pessimistic (Left)
              -- case.  The underlying problem is that we compute the maximum
              -- possible block size pre-layout by calling this function with a
              -- fake jump target (since we don't know where the real target
              -- will ultimately be yet).  If we did not add the no-ops here to
              -- preserve the size of the block, the layout code would be
              -- required to add padding instructions at the end of the block to
              -- keep the layout consistent.  This is a problem in block that
              -- have a fallthrough successor (e.g., this case), as it would
              -- insert traps between the conditional branch and the
              -- fallthrough, causing crashes.  Long story short, we add no-ops
              -- to preserve the integrity of the instruction address layout.
              let nop = I (D.Instruction D.ORI (    D.Annotated () (D.Gprc (D.GPR 0))
                                               D.:< D.Annotated () (D.U16imm 0)
                                               D.:< D.Annotated () (D.Gprc (D.GPR 0))
                                               D.:< D.Nil
                                               )
                          )
              return (         nop
                      DLN.:| [ nop
                             , I (D.Instruction opc (D.Annotated a (D.Condbrtarget (D.CBT (tgtOff4 `shiftR` 2))) D.:< rest))
                             ]
                     )
            Left _ -> do
              -- Otherwise, the target is too far away for a conditional branch.
              -- Instead, we'll conditionally branch to an unconditional branch
              -- that takes us to the desired target.
              --
              -- > bc +4      ; Skip the next instruction to the long jump
              -- > b +4       ; Skip the next instruction (going to the natural fallthrough)
              -- > b <target> ; Long jump to the actual target
              --
              -- Note: the branch values for the first two instructions are 2 because:
              --
              -- 1. The jump offset encoded in the instruction has 0b00
              --    concatenated as the low bits in the CPU (equivalent to shift
              --    left by two)
              -- 2. The offset must also skip the instruction it is executing
              --    (i.e., br 0 is an infinite loop)
              --
              -- NOTE: The offset is computed at an offset of 2 from the first
              -- instruction we generate
              off <- absoluteOff 2 targetAddr
              return (        I (D.Instruction opc (D.Annotated a (D.Condbrtarget (D.CBT 2)) D.:< rest))
                     DLN.:| [ I (D.Instruction D.B (D.Annotated () (D.Directbrtarget (D.BT 2)) D.:< D.Nil))
                            , I (D.Instruction D.B (D.Annotated () (D.Directbrtarget off) D.:< D.Nil))
                            ]
                     )
        _ -> RP.panic RP.PPCISA "ppcModifyJumpTarget" [ "Unexpected branch type: " ++ show opc
                                                      , "  allocated to address: " ++ show srcAddr
                                                      , "  jumping to address:   " ++ show targetAddr
                                                      ]
  where
  die :: String -> a
  die s = RP.panic RP.PPCISA "ppcModifyJumpTarget"
    [ s
    , "Address: " ++ show srcAddr
    , "Instruction: " ++ ppcPrettyInstruction i
    ]
  -- This @n@ is the index of the generated instruction that the computed offset
  -- will be used from.  For example, if the offset will be used in the first
  -- instruction generated by 'ppcModifyJumpTarget', @n = 0@.  If it is to be
  -- used in the third instruction, @n = 2@.
  absoluteOff n addr = case newJumpOffset 26 (R.addressAddOffset srcAddr (4*n)) addr of
    Left err -> die err
    Right off4 -> Just (D.BT (off4 `shiftR` 2))

-- | Compute a new jump offset between the @srcAddr@ and @targetAddr@.
--
-- If the new offset exceeds what is reachable with a single branch instruction,
-- call error.  The limit of the branch is specified as @nBits@, which is the
-- number of bits in the immediate field that will hold the offset.  Note that
-- offsets are signed, so the range check has to account for that.
newJumpOffset :: (HasCallStack, MM.MemWidth (MM.ArchAddrWidth arch)) => Int -> R.ConcreteAddress arch -> R.ConcreteAddress arch -> Either String Int32
newJumpOffset nBits srcAddr targetAddr
  | rawOff `mod` 4 /= 0 =
    Left (printf "Invalid alignment for offset between src=%s and target=%s" (show srcAddr) (show targetAddr))
  | rawOff >= 2^(nBits - 1) || rawOff <= negate (2^(nBits - 1)) =
    Left (printf "Jump offset too large between src=%s and target=%s" (show srcAddr) (show targetAddr))
  | otherwise = Right (fromIntegral rawOff)
  where
    rawOff = targetAddr `R.addressDiff` srcAddr

-- | This is an are orphan instance; is there a better place to put it?
instance R.ToGenericInstruction (MP.AnyPPC v) where
  toGenericInstruction   = toInst
  fromGenericInstruction = fromInst


-- | Convert the 'Instruction' wrapper to the base instruction type, dropping
-- annotations. This operation is depricated in favor of
-- 'R.toGenericInstruction'.
--
-- Note that the coercion of the opcode is safe, because the second type
-- parameter is phantom.
toInst :: Instruction tp a -> D.Instruction
toInst i =
  case unI i of
    D.Instruction opc annotatedOps ->
      D.Instruction (coerce opc) (FC.fmapFC unannotateOpcode annotatedOps)

-- | Convert the base instruction type to the wrapped 'Instruction' with a unit
-- annotation. This operation is depricated in favor of
-- 'R.fromGenericInstruction'.
fromInst :: D.Instruction -> Instruction tp ()
fromInst i =
  case i of
    D.Instruction opc unannotatedOps ->
      I (D.Instruction (coerce opc) (FC.fmapFC (D.Annotated ()) unannotatedOps))

unannotateOpcode :: D.Annotated a D.Operand tp -> D.Operand tp
unannotateOpcode (D.Annotated _ op) = op

annotateInstr :: Instruction tp () -> a -> Instruction tp a
annotateInstr (I i) a =
  case i of
    D.Instruction opc operands ->
      I (D.Instruction (coerce opc) (FC.fmapFC (\(D.Annotated _ op) -> D.Annotated a op) operands))

{- Note [Conditional Branch Restrictions]

On PowerPC, conditional branches only have 16 bits of offset (14 physical, two
implied), which is only enough to jump within a 64kb region.  This isn't enough
for very reasonable binaries, and the rewriter fails when trying to rewrite the
jumps directly.

The fix is to change the code sequence we generate to incorporate some
unconditional jumps, which have 26 bits of offset (24 bits physical) available.

Assume we have the following original instruction sequence where the jump is out
of range for a conditional branch:

#+BEGIN_SRC asm
bdnz cr3,<target>
#+END_SRC

We can rewrite this to do the actual jumping through longer unconditional jumps:

#+BEGIN_SRC asm
bdnz cr3,8
b 8
b <target>
#+END_SRC

There are two cases:

1. We take the conditional branch

   In this case, we skip to the ~b <target>~ instruction, which does an
   unconditional jump with a longer range.

2. We don't take the conditional branch and need to fall through

   In this case, we fall through to a ~b 8~ instruction, which then jumps past
   our jump from case (1) and to the natural fallthrough

-}
