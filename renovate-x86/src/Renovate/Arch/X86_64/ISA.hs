{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ViewPatterns #-}
-- | The 'ISA' for x86_64
module Renovate.Arch.X86_64.ISA (
  isa,
  assemble,
  disassemble
  ) where

import qualified GHC.Err.Located as L

import           Control.Applicative ( Alternative, empty )
import qualified Control.Monad.Catch as C
import           Data.Bits ( bit )
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LB
import qualified Data.Functor.Identity as I
import           Data.Int ( Int32 )
import qualified Data.List.NonEmpty as DLN
import           Data.Parameterized.NatRepr
import           Data.Parameterized.Some
import qualified Prettyprinter as PD
import           Data.Void ( absurd )
import           Data.Word ( Word8, Word64 )

import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.X86 as X86
import qualified Flexdis86 as D

import qualified Renovate as R
import           Renovate.Arch.X86_64.Internal
import qualified Renovate.Arch.X86_64.Panic as RP

-- | Assemble an instruction into a 'B.ByteString'
--
-- This can fail with exceptions of type 'AssemblyFailure'.  These can
-- be obtained by instantiating @m@ as either 'Either' or 'IO'.
assemble :: forall (tp :: R.InstructionArchReprKind X86.X86_64) m
          . (C.MonadThrow m)
         => Instruction tp ()
         -> m B.ByteString
assemble i =
  case i of
    XI ii -> case D.assembleInstruction (fmap aoOperand ii) of
      Nothing -> C.throwM (InstructionAssemblyFailure i)
      Just bldr -> return (LB.toStrict (BB.toLazyByteString bldr))
    RawBytes b -> return b

-- | Disassemble a single instruction from a 'B.ByteString' and return
-- the instruction and remaining bytes.
disassemble :: (C.MonadThrow m)
            => MD.ParsedBlock X86.X86_64 ids
            -> R.ConcreteAddress X86.X86_64
            -> R.ConcreteAddress X86.X86_64
            -> B.ByteString
            -> m (R.ConcreteBlock X86.X86_64)
disassemble pb start end b0 = do
  insns0 <- go 0 start b0 []
  case DLN.nonEmpty insns0 of
    Nothing -> C.throwM (EmptyBlock start)
    Just insns -> return (R.concreteBlock start insns X86Repr pb)
  where
    go totalRead insnAddr b insns =
      case D.tryDisassemble b of
        (bytesRead, Nothing) ->
          C.throwM (InstructionDisassemblyFailure1 b0 (totalRead + bytesRead))
        (bytesRead, Just ii) ->
          let nextAddr = insnAddr `R.addressAddOffset` fromIntegral bytesRead
          in if | nextAddr > end ->
                  -- We have parsed an instruction that crosses a block boundary. We
                  -- should probably give up -- this executable is too wonky.
                  C.throwM (OverlappingBlocks insnAddr nextAddr end)
                | nextAddr == end ->
                  -- The next instruction we would decode starts another
                  -- block, OR the instruction we just decoded is a
                  -- terminator, so end the block and stop decoding
                  return (reverse (fromFlexInst X86Repr ii : insns))
                | otherwise ->
                  -- Otherwise, we just keep decoding
                  go (totalRead + bytesRead) nextAddr (B.drop bytesRead b) (fromFlexInst X86Repr ii : insns)

-- | An implementation of the 'ISA' for x86_64.
--
-- This particular implementation uses @int 3@ for padding and encodes
-- jumps as @push ADDR ; ret@.
isa :: R.ISA X86.X86_64
isa = R.ISA
  { R.isaInstructionSize = x64Size
  , R.isaJumpType = x64JumpType
  , R.isaInstructionArchReprs = R.SomeInstructionArchRepr X86Repr DLN.:| []
  , R.isaInstructionRepr = instructionRepr
  , R.isaMakeRelativeJumpTo = x64MakeRelativeJumpTo
  , R.isaMaxRelativeJumpSize = const (bit 31 - 1)
  , R.isaMakePadding = x64MakePadding
  , R.isaSymbolizeAddresses = x64SymbolizeAddresses
  , R.isaConcretizeAddresses = x64ConcretizeAddresses
  , R.isaPrettyInstruction = show . PD.pretty
  }

-- | Simple adapter for 'x64JumpTypeRaw' with the right type to be used in an
-- ISA.
x64JumpType :: forall (tp :: R.InstructionArchReprKind X86.X86_64) t unused
             . Instruction tp t
            -> MM.Memory 64
            -> R.ConcreteAddress X86.X86_64
            -> unused
            -> Some (R.JumpType X86.X86_64)
x64JumpType insn _ addr _ = x64JumpTypeRaw insn addr

-- | Classify different kinds of jump instructions.
--
-- Fortunately, all of the conditional jumps start with 'j', and no
-- other instructions start with 'j'.  This lets us have an easy case
-- to handle all of them.
x64JumpTypeRaw :: forall (tp :: R.InstructionArchReprKind X86.X86_64) t
                . Instruction tp t
               -> R.ConcreteAddress X86.X86_64
               -> Some (R.JumpType X86.X86_64)
x64JumpTypeRaw (RawBytes _) _ =
  -- NOTE: This could be a panic: none of these should show up until a user inserts them
  Some R.NoJump
x64JumpTypeRaw xi@(XI ii) addr =
  case (BSC.unpack (D.iiOp ii), map (fst . aoOperand) (D.iiArgs ii)) of
    ("jmp", [D.JumpOffset _ off]) -> Some (R.RelativeJump R.Unconditional addr (fixJumpOffset sz off))
    ("jmp", _) -> Some (R.IndirectJump R.Unconditional)
    ("ret", _) -> Some (R.Return R.Unconditional)
    ('i':'n':'t':_, _) -> Some R.IndirectCall
    ('i':'r':'e':'t':_, _) -> Some (R.Return R.Unconditional)
    ('l':'o':'o':'p':_, [D.JumpOffset _ off]) -> Some (R.RelativeJump R.Conditional addr (fixJumpOffset sz off))
    ('j':_, [D.JumpOffset _ off]) -> Some (R.RelativeJump R.Conditional addr (fixJumpOffset sz off))
    -- We treat calls as conditional jumps for the purposes of basic
    -- block recovery.  The important aspect of this treatment is that
    -- execution can (does) reach the instruction after the call, and
    -- that lets basic block discovery find the instructions that
    -- execute on return.
    ("call", [D.JumpOffset _ off]) ->
      -- We used to worry about jumps from "main" back to the pre-main
      -- code added by the compiler to initialize the CRT.  With
      -- Macaw, we can just analyze from _start and be okay, so we
      -- don't have to worry about this odd type of jump anymore
      let offset = fixJumpOffset sz off
      in Some (R.DirectCall addr offset)
    ("call", _) -> Some R.IndirectCall
    ("syscall", _) -> Some R.IndirectCall
    _ -> Some R.NoJump
  where
    sz = x64Size xi

-- | This function corrects for a difference in representation between
-- x86_64 and the Renovate redirection code.  The redirection code assumes
-- that the instruction pointer (IP) points to the beginning of an
-- instruction as its reference point.  In x86_64, the IP points to
-- the beginning of the *next* instruction when the current
-- instruction is executing.  To get our jumps to point to the right
-- place, we have to add the size of the jump instruction to the
-- offset for relative jumps.
fixJumpOffset :: (MM.MemWidth w) => Word8 -> D.JumpOffset -> MM.MemWord w
fixJumpOffset sz (D.FixedOffset off) = fromIntegral (off + fromIntegral sz)
fixJumpOffset _ o@(D.RelativeOffset {}) = error ("Relative offsets for relocations are not yet supported: " ++ show o)

-- | Encode a relative jump using a 32 bit immediate.  This gives us a maximum range of
-- +/- 2GB.
--
-- See Note [RelativeJump]
x64MakeRelativeJumpTo :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
                       . R.ConcreteAddress X86.X86_64
                      -> R.ConcreteAddress X86.X86_64
                      -> R.InstructionArchRepr X86.X86_64 tp
                      -> DLN.NonEmpty (Instruction tp ())
x64MakeRelativeJumpTo srcAddr targetAddr repr
  | abs jmpOffset > i32Max =
    L.error ("Relative branch is out of range: from " ++ show srcAddr ++
             " to " ++ show targetAddr)
  | jmpInstrSizeGuess /= jmpInstrSize =
    L.error ("BUG! The 'jmp' instruction size is not as expected! " ++
             "Expected " ++ show jmpInstrSizeGuess ++ " but got " ++
             show jmpInstrSize)
  | otherwise = jmpInstr DLN.:| []
  where
    jmpInstr :: Instruction tp ()
    jmpInstr = makeInstr repr "jmp"
      [D.JumpOffset D.JSize32 (D.FixedOffset (fromIntegral jmpOffset))]
    jmpInstrSize = fromIntegral $ x64Size jmpInstr
    -- We need to know the size of the generated jump instruction to
    -- generate it, so we guess now and check that we were right after
    -- generation.
    jmpInstrSizeGuess :: Int
    jmpInstrSizeGuess = 5
    -- Our ISA independent is that jumps start at the beginning of the
    -- current instruction; see Note [RelativeJump]. Here we are going
    -- from ISA independent addresses to x86 ISA addresses, and so
    -- we *subtract* the jump instruction size in the offset. Compare
    -- with the symmetric code above in 'x64JumpType', which uses
    -- 'fixJumpOffset' to *add* the jump instruction size, because
    -- that code is translating from x86 ISA addresses to ISA
    -- independent addresses.
    jmpOffset :: Integer
    jmpOffset = fromIntegral $ (targetAddr `R.addressDiff` srcAddr) - fromIntegral jmpInstrSizeGuess
    i32Max :: Integer
    i32Max = fromIntegral (maxBound :: Int32)

-- | Make @n@ bytes of @int 3@ instructions.  If executed, these
-- generate an interrupt that drops the application into a debugger
-- (or terminates it).
--
-- An alternative would be to use the two-byte undefined instruction
-- @ud2@ where possible and pad the rest with an @int 3@.
x64MakePadding :: Word64 -> R.InstructionArchRepr X86.X86_64 tp -> [Instruction tp ()]
x64MakePadding nBytes repr =
  replicate (fromIntegral nBytes) (makeInstr repr "int3" [])

addrRefToAddress :: (D.AddrRef -> R.Relocation X86.X86_64) -> D.Value -> R.Relocation X86.X86_64
addrRefToAddress f v =
  case v of
    D.FarPointer a -> f a
    D.VoidMem a -> f a
    D.Mem8 a -> f a
    D.Mem16 a -> f a
    D.Mem32 a -> f a
    D.Mem64 a -> f a
    D.Mem128 a -> f a
    D.FPMem32 a -> f a
    D.FPMem64 a -> f a
    D.FPMem80 a -> f a
    _ -> R.NoRelocation

ripToAbs :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
          . MM.Memory 64
         -> R.ConcreteAddress X86.X86_64
         -> Instruction tp ()
         -> D.AddrRef
         -> R.Relocation X86.X86_64
ripToAbs mem iStartAddr i ref =
  case ref of
    D.IP_Offset_32 _seg disp -> absoluteDisplacement mem iEndAddr disp
    D.IP_Offset_64 _seg disp -> absoluteDisplacement mem iEndAddr disp
    _ -> R.NoRelocation
  where
    iEndAddr = iStartAddr `R.addressAddOffset` fromIntegral (x64Size i)

absoluteDisplacement :: MM.Memory 64 -> R.ConcreteAddress X86.X86_64 -> D.Displacement -> R.Relocation X86.X86_64
absoluteDisplacement _mem endAddr disp =
  case disp of
    D.NoDisplacement -> L.error "Unexpected NoDisplacement"
    D.Disp8  d       -> R.PCRelativeRelocation (endAddr `R.addressAddOffset` fromIntegral d)
    D.Disp32 (D.Imm32Concrete d)       -> R.PCRelativeRelocation (endAddr `R.addressAddOffset` fromIntegral d)
    D.Disp32 (D.Imm32SymbolOffset {}) -> L.error "Symbolic references are not supported"

-- | If given a jump, promote it to a 4 byte displacement
promoteJump
  :: D.InstructionInstanceF (AnnotatedOperand ())
  -> D.InstructionInstanceF (AnnotatedOperand ())
promoteJump ii =
  case (BSC.unpack (D.iiOp ii), D.iiArgs ii) of
    ('j' : _, [AnnotatedOperand (D.JumpOffset _ val, _ty) ()]) ->
      case D.mkInstruction (BSC.unpack (D.iiOp ii)) [D.JumpOffset D.JSize32 val] of
        Just ii' -> ii' { D.iiArgs = fmap (\o -> AnnotatedOperand o ()) (D.iiArgs ii') }
        Nothing -> RP.panic RP.X86_64ISA "promoteJump" [ "Unable to promote jump " ++ show ii ]
    _ -> ii

-- | Needs to convert all Disp8 IP relative references to Disp32
--
-- NOTE: The jump instructions don't have any IP-relative memory address
-- references, so we don't need to update any IP-relative address operands.
-- However, we do need to extend any jumps we find into jumps with 4 byte
-- offsets.
x64SymbolizeAddresses :: forall (tp :: R.InstructionArchReprKind X86.X86_64) ids
                       . MM.Memory 64
                      -> (R.ConcreteAddress X86.X86_64 -> R.SymbolicAddress X86.X86_64)
                      -> MD.ParsedBlock X86.X86_64 ids
                      -> R.ConcreteAddress X86.X86_64
                      -> Instruction tp ()
                      -> [R.Instruction X86.X86_64 tp (R.Relocation X86.X86_64)]
x64SymbolizeAddresses _ _ _ _ (RawBytes b) = [RawBytes b]
x64SymbolizeAddresses mem toSymbolic pb insnAddr i0@(XI (promoteJump -> ii)) =
  [XI (ii { D.iiArgs = fmap (toRelocatableOperand mem toSymbolic pb insnAddr ii size0) (D.iiArgs ii) })]
  where
    size0 = x64Size i0

toRelocatableOperand
  :: MM.Memory 64
  -> (R.ConcreteAddress X86.X86_64 -> R.SymbolicAddress X86.X86_64)
  -> MD.ParsedBlock X86.X86_64 ids
  -> R.ConcreteAddress X86.X86_64
  -> D.InstructionInstanceF (AnnotatedOperand ())
  -> Word8
  -- ^ The original size of the instruction (before jump promotion), which is
  -- needed to calculate jump targets
  -> AnnotatedOperand ()
  -> AnnotatedOperand (R.Relocation X86.X86_64)
toRelocatableOperand mem toSymbolic _pb insnAddr ii size0 AnnotatedOperand { aoOperand = (v, ty) } =
  case v of
    D.JumpOffset _ oldOffset ->
      -- Note that jump offsets are always extended to 32 bit offsets so that we
      -- have consistent instruction sizing no matter how we re-arrange code
      --
      -- We use the original instruction size (pre-promotion) to compute target
      -- offsets correctly
      let targetAddr = insnAddr `R.addressAddOffset` fixJumpOffset size0 oldOffset
      in AnnotatedOperand { aoOperand = (D.JumpOffset D.JSize32 (D.FixedOffset 0), D.OpType D.JumpImmediate D.ZSize)
                          , aoAnnotation = R.SymbolicRelocation (toSymbolic targetAddr)
                          }
    _ ->
      AnnotatedOperand { aoOperand = (I.runIdentity (mapAddrRef promoteRipDisp8 I.Identity v), ty)
                       , aoAnnotation = addrRefToAddress (ripToAbs mem insnAddr (XI ii)) v
                       }

promoteRipDisp8 :: D.AddrRef -> I.Identity D.AddrRef
promoteRipDisp8 ref =
  let toD32 = D.Disp32 . D.Imm32Concrete . fromIntegral in
  case ref of
    D.IP_Offset_32 seg (D.Disp8 d) -> I.Identity $ D.IP_Offset_32 seg $ toD32 d
    D.IP_Offset_64 seg (D.Disp8 d) -> I.Identity $ D.IP_Offset_64 seg $ toD32 d
    _ -> I.Identity ref

-- | This function fixes up memory references in operands as well as targets of
-- relative jumps
--
-- Note that on x86_64, an instruction *either* has memory reference operands or
-- is a jump, but never both.  Thus, we have a top-level dispatch that chooses
-- between the two cases and concretizes appropriately.
x64ConcretizeAddresses :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
                        . (L.HasCallStack)
                       => MM.Memory 64
                       -> (R.SymbolicAddress X86.X86_64 -> R.ConcreteAddress X86.X86_64)
                       -> R.ConcreteAddress X86.X86_64
                       -- ^ The address assigned to this instruction in the new
                       -- layout
                       -> Instruction tp (R.Relocation X86.X86_64)
                       -- ^ The instruction to concretize
                       -> DLN.NonEmpty (Instruction tp ())
x64ConcretizeAddresses _ _ _ (RawBytes b) = RawBytes b DLN.:| []
x64ConcretizeAddresses mem toConcrete insnAddr (XI ii) =
  XI (ii { D.iiArgs = fmap (resolveRelocations mem toConcrete insnAddr ii) (D.iiArgs ii) }) DLN.:| []

resolveRelocations
  :: MM.Memory 64
  -> (R.SymbolicAddress X86.X86_64 -> R.ConcreteAddress X86.X86_64)
  -> R.ConcreteAddress X86.X86_64
  -> D.InstructionInstanceF (AnnotatedOperand (R.Relocation X86.X86_64))
  -> AnnotatedOperand (R.Relocation X86.X86_64)
  -> AnnotatedOperand ()
resolveRelocations mem toConcrete insnAddr ii AnnotatedOperand { aoOperand = (v, ty)
                                                               , aoAnnotation = reloc
                                                               } =
  case reloc of
    R.ArchRelocation ext -> absurd ext
    R.NoRelocation -> mkUnitAnnot (v, ty)
    R.PCRelativeRelocation absAddr
      | Just v' <- mapAddrRef (absToRip mem insnAddr (XI ii) absAddr) (updateNonMemoryOperand (XI ii) insnAddr absAddr) v ->
        mkUnitAnnot (v', ty)
      | otherwise ->
        RP.panic RP.X86_64ISA "resolveRelocations" [ "Unexpected relocation on operand: " ++ show (absAddr, v)
                                                   , "  in instruction " ++ show ii
                                                   ]
    -- Note that we currently only use symbolic addresses for jump targets
    R.SymbolicRelocation symAddr ->
      let repr = instructionRepr (XI ii)
          opcode = BSC.unpack (D.iiOp ii)
          fakeJmp = makeInstr repr opcode [D.JumpOffset D.JSize32 (D.FixedOffset 0)]
          jmpOff = toInteger (toConcrete symAddr `R.addressDiff` insnAddr)
          jmpSize = toInteger (x64Size fakeJmp)
          correctedOffset = jmpOff - jmpSize
      in case v of
        D.JumpOffset _sz (D.FixedOffset _)
          | abs correctedOffset > fromIntegral (maxBound @Int32) ->
            RP.panic RP.X86_64ISA "resolveRelocations" [ " Relative branch is out of range: from " ++ show insnAddr
                                                       , "   to " ++ show (toConcrete symAddr)
                                                       ]
          | otherwise -> mkUnitAnnot (D.JumpOffset D.JSize32 (D.FixedOffset (fromIntegral correctedOffset)), D.OpType D.JumpImmediate D.ZSize)
        _ -> mkUnitAnnot (v, ty)

-- | Update operands that have an 'AbsoluteAddress' annotation, but that are not memory operands
--
-- Right now, this is *only* jumps (or calls) to concrete addresses in the form
-- of 'D.JumpOffset' operands
updateNonMemoryOperand
  :: (Alternative f)
  => Instruction tp (R.Relocation X86.X86_64)
  -> R.ConcreteAddress X86.X86_64
  -> R.ConcreteAddress X86.X86_64
  -> D.Value
  -> f D.Value
updateNonMemoryOperand i iStartAddr targetAddr v =
  case v of
    D.JumpOffset D.JSize32 (D.FixedOffset _) ->
      pure (D.JumpOffset D.JSize32 (D.FixedOffset off))
    _ -> empty
  where
    iEndAddr = iStartAddr `R.addressAddOffset` fromIntegral (x64Size i)
    off = fromIntegral (targetAddr `R.addressDiff` iEndAddr)

mapAddrRef :: (Applicative f) => (D.AddrRef -> f D.AddrRef) -> (D.Value -> f D.Value) -> D.Value -> f D.Value
mapAddrRef f ifNotMem v =
  case v of
    D.FarPointer a -> D.FarPointer <$> f a
    D.VoidMem a -> D.VoidMem <$> f a
    D.Mem8 a -> D.Mem8 <$> f a
    D.Mem16 a -> D.Mem16 <$> f a
    D.Mem32 a -> D.Mem32 <$> f a
    D.Mem64 a -> D.Mem64 <$> f a
    D.Mem128 a -> D.Mem128 <$> f a
    D.FPMem32 a -> D.FPMem32 <$> f a
    D.FPMem64 a -> D.FPMem64 <$> f a
    D.FPMem80 a -> D.FPMem80 <$> f a
    _ -> ifNotMem v

-- FIXME: Guard these arithmetic operations from overflows
absToRip :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
          . MM.Memory 64
         -> R.ConcreteAddress X86.X86_64
         -> Instruction tp (R.Relocation X86.X86_64)
         -> R.ConcreteAddress X86.X86_64
         -> D.AddrRef
         -> Maybe D.AddrRef
absToRip _mem iStartAddr i a ref =
  case ref of
    D.IP_Offset_32 seg (D.Disp32 _) -> Just $ D.IP_Offset_32 seg d32
    D.IP_Offset_64 seg (D.Disp32 _) -> Just $ D.IP_Offset_64 seg d32
    _ -> Nothing
  where
    iEndAddr = iStartAddr `R.addressAddOffset` fromIntegral (x64Size i)
    d32 = D.Disp32 (D.Imm32Concrete (fromIntegral (a `R.addressDiff` iEndAddr)))

-- ipDisplacement :: Word64 -> D.Displacement -> D.Displacement
-- ipDisplacement iEndAddr disp =
--   case disp of
--     D.NoDisplacement -> D.NoDisplacement
--     D.Disp8 d -> error ("Disp8 in unexpected place")
--     D.Disp32 d -> D.Disp32 (d - fromIntegral iEndAddr)


-- for mul/imul, we can look at @ii@ and examine the *DX register for
-- the single operand versions.  The multi-operand versions of the mul
-- instructions set the flags as normal

{- Note [Instruction Format]

The InstructionInstance type is provided by flexdis86.  Normally,
flexdis86 wraps this type in a DisassembledAddress type, which records
the address and size of the instruction.  The InstructionInstance
actually has no idea about the real size of the instruction.

As part of the work to be able to re-assemble opcodes, we will need to
know the precise encoding of each InstructionInstance.  This will also
let us compute a size for each instruction easily.

-}

{- Note [RelativeJump]

Relative jumps on x86 are computed relative to the IP, which is
incremented to the next instruction after execution of the jmp starts.
That means we have to subtract off 5 bytes here (1 for the jmp opcode
and 4 for the jmp immediate).

-}

