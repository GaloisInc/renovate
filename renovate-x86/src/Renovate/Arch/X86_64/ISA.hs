{-# LANGUAGE FlexibleContexts #-}
-- | The 'ISA' for x86_64
module Renovate.Arch.X86_64.ISA (
  isa,
  assemble,
  disassemble,
  disassemble1
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Functor.Identity as I
import qualified Data.Text.Prettyprint.Doc as PD
import Data.Int ( Int32, Int64 )
import Data.Word ( Word8, Word64 )
import qualified Flexdis86 as D

import qualified Data.Macaw.Memory as MM

import Renovate
import Renovate.Arch.X86_64.Internal

-- | Assemble an instruction into a 'B.ByteString'
--
-- This can fail with exceptions of type 'AssemblyFailure'.  These can
-- be obtained by instantiating @m@ as either 'Either' or 'IO'.
assemble :: (C.MonadThrow m) => Instruction () -> m B.ByteString
assemble i =
  case D.assembleInstruction (toFlexInst i) of
    Nothing -> C.throwM (InstructionAssemblyFailure i)
    Just bldr -> return (LB.toStrict (B.toLazyByteString bldr))

-- | Disassemble a 'B.ByteString' into concrete instructions (with no
-- annotation)
--
-- This can fail with exceptions of type 'DisassemblyFailure'.
disassemble :: (C.MonadThrow m) => B.ByteString -> m [Instruction ()]
disassemble = mapM checkDisassembly . D.disassembleBuffer
  where
    checkDisassembly disAddr =
      case D.disInstruction disAddr of
        Nothing -> C.throwM (InstructionDisassmblyFailure (D.disOffset disAddr) (D.disLen disAddr))
        Just i -> return (fromFlexInst i)

-- | Disassemble a single instruction from a 'B.ByteString' and return
-- the instruction and remaining bytes.
disassemble1 :: (C.MonadThrow m) => B.ByteString -> m (Int, Instruction ())
disassemble1 b =
  case mii of
    Nothing -> C.throwM (InstructionDisassmblyFailure1 b bytesRead)
    Just i -> return (bytesRead, fromFlexInst i)
  where
    (bytesRead, mii) = D.tryDisassemble b

-- | An implementation of the 'ISA' for x86_64.
--
-- This particular implementation uses @int 3@ for padding and encodes
-- jumps as @push ADDR ; ret@.
isa :: (MM.MemWidth w) => ISA Instruction (TargetAddress w) w
isa =
  ISA { isaInstructionSize = x64Size
      , isaJumpType = x64JumpType
      , isaMakeRelativeJumpTo = x64MakeRelativeJumpTo
      , isaModifyJumpTarget = x64ModifyJumpTarget
      , isaMakePadding = x64MakePadding
      , isaMakeTrapIf = x64MakeTrapIf
      , isaSymbolizeAddresses = x64SymbolizeAddresses
      , isaConcretizeAddresses = x64ConcretizeAddresses
      , isaMakeSymbolicJump = x64MakeSymbolicJump
      , isaPrettyInstruction = show . PD.pretty
      }

-- | Classify different kinds of jump instructions.
--
-- Fortunately, all of the conditional jumps start with 'j', and no
-- other instructions start with 'j'.  This lets us have an easy case
-- to handle all of them.
x64JumpType :: (MM.MemWidth w) => Instruction t -> MM.Memory w -> RelAddress w -> JumpType w
x64JumpType xi@(XI ii) _mem addr =
  case (D.iiOp ii, map (fst . aoOperand) (D.iiArgs ii)) of
    ("jmp", [D.JumpOffset _ off]) -> RelativeJump Unconditional addr (fixJumpOffset sz off)
    ("jmp", _) -> IndirectJump Unconditional
    ("ret", _) -> Return
    ('i':'n':'t':_, _) -> IndirectCall
    ('i':'r':'e':'t':_, _) -> Return
    ('l':'o':'o':'p':_, [D.JumpOffset _ off]) -> RelativeJump Conditional addr (fixJumpOffset sz off)
    ('j':_, [D.JumpOffset _ off]) -> RelativeJump Conditional addr (fixJumpOffset sz off)
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
      in DirectCall addr offset
    ("call", _) -> IndirectCall
    ("syscall", _) -> IndirectCall
    _ -> NoJump
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
fixJumpOffset :: (MM.MemWidth w) => Word8 -> Int64 -> MM.MemWord w
fixJumpOffset sz off = fromIntegral (off + fromIntegral sz)

-- | Encode a relative jump using a 32 bit immediate.  This gives us a maximum range of
-- +/- 2GB.
--
-- See Note [RelativeJump]
x64MakeRelativeJumpTo :: (MM.MemWidth w) => RelAddress w -> RelAddress w -> [Instruction ()]
x64MakeRelativeJumpTo srcAddr targetAddr
  | abs jmpOffset > i32Max =
    L.error ("Relative branch is out of range: from " ++ show srcAddr ++
             " to " ++ show targetAddr)
  | jmpInstrSizeGuess /= jmpInstrSize =
    L.error ("BUG! The 'jmp' instruction size is not as expected! " ++
             "Expected " ++ show jmpInstrSizeGuess ++ " but got " ++
             show jmpInstrSize)
  | otherwise = [jmpInstr]
  where
    jmpInstr = makeInstr "jmp"
      [D.JumpOffset D.ZSize (fromIntegral jmpOffset)]
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
    jmpOffset = fromIntegral $ (targetAddr `addressDiff` srcAddr) - fromIntegral jmpInstrSizeGuess
    i32Max :: Integer
    i32Max = fromIntegral (maxBound :: Int32)

x64MakeSymbolicJump :: (MM.MemWidth w) => SymbolicAddress -> [TaggedInstruction Instruction (TargetAddress w)]
x64MakeSymbolicJump sa = [tagInstruction (Just sa) i]
  where
    i = annotateInstr (makeInstr "jmp" [off]) NoAddress
    off = D.JumpOffset D.ZSize 0

x64ModifyJumpTarget :: (MM.MemWidth w) => Instruction () -> RelAddress w -> RelAddress w -> Maybe [Instruction ()]
x64ModifyJumpTarget (XI ii) srcAddr targetAddr
  | abs jmpOffset > i32Max =
    L.error ("Relative branch is out of range: from " ++ show srcAddr ++
             " to " ++ show targetAddr)
  | jmpInstrSizeGuess /= jmpInstrSize =
    L.error ("BUG! The 'jmp' instruction size is not as expected! " ++
             "Expected " ++ show jmpInstrSizeGuess ++ " but got " ++
             show jmpInstrSize)
  | 'j' : _ <- D.iiOp ii = Just [extendDirectJump]
  | 'c' : 'a' : 'l' : 'l' : _ <- D.iiOp ii = Just [extendDirectJump]
  | otherwise = Nothing
  where jmpOffset :: Integer
        jmpOffset = fromIntegral $ (targetAddr `addressDiff` srcAddr) - fromIntegral jmpInstrSizeGuess
        i32Max :: Integer
        i32Max = fromIntegral (maxBound :: Int32)

        -- Make a dummy instruction of the correct type to get a size
        -- guess.  This should never fail the size check.  We have to
        -- generate an instruction and check its size because some jumps
        -- could be 5 bytes, while others could be 6.
        dummyJumpInstr = makeInstr (D.iiOp ii) [D.JumpOffset D.ZSize 0]
        jmpInstrSizeGuess = x64Size dummyJumpInstr
        jmpInstr = makeInstr (D.iiOp ii) [D.JumpOffset D.ZSize (fromIntegral jmpOffset)]
        jmpInstrSize = fromIntegral $ x64Size jmpInstr

        extendDirectJump = case aoOperand <$> D.iiArgs ii of
          [(D.JumpOffset _ _, _)] -> jmpInstr
          _ -> XI ii

-- | Make @n@ bytes of @int 3@ instructions.  If executed, these
-- generate an interrupt that drops the application into a debugger
-- (or terminates it).
--
-- An alternative would be to use the two-byte undefined instruction
-- @ud2@ where possible and pad the rest with an @int 3@.
x64MakePadding :: Word64 -> [Instruction ()]
x64MakePadding nBytes =
  replicate (fromIntegral nBytes) (makeInstr "int3" [])

-- | Use jno and jnc to handle signed and unsigned overflow,
-- respectively.
--
-- Currently uses 'int3' as the halt instruction.  Other choices could
-- be reasonable.
x64MakeTrapIf :: Instruction (TargetAddress w) -> TrapPredicate -> [Instruction (TargetAddress w)]
x64MakeTrapIf _ii tp =
  case tp of
    SignedOverflow -> [ annotateInstr (makeInstr "jno" [jmpOff]) NoAddress
                      , annotateInstr trap NoAddress
                      ]
    UnsignedOverflow -> [ annotateInstr (makeInstr "jnc" [jmpOff]) NoAddress
                        , annotateInstr trap NoAddress
                        ]
  where
    trap = makeInstr "int3" []
    jmpOff = D.JumpOffset D.ZSize (fromIntegral (x64Size trap))

addrRefToAddress :: (D.AddrRef -> TargetAddress w) -> D.Value -> TargetAddress w
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
    _ -> NoAddress

ripToAbs :: (MM.MemWidth w) => MM.Memory w -> RelAddress w -> Instruction () -> D.AddrRef -> TargetAddress w
ripToAbs mem iStartAddr i ref =
  case ref of
    D.IP_Offset_32 _seg disp -> absoluteDisplacement mem iEndAddr disp
    D.IP_Offset_64 _seg disp -> absoluteDisplacement mem iEndAddr disp
    _ -> NoAddress
  where
    addOff   = addressAddOffset mem
    iEndAddr = iStartAddr `addOff` fromIntegral (x64Size i)

absoluteDisplacement :: (MM.MemWidth w) => MM.Memory w -> RelAddress w -> D.Displacement -> TargetAddress w
absoluteDisplacement mem endAddr disp =
  case disp of
    D.NoDisplacement -> L.error "Unexpected NoDisplacement"
    D.Disp8  d       -> AbsoluteAddress (endAddr `addOff` fromIntegral d) -- (Address (fromIntegral d + fromIntegral endAddr))
    D.Disp32 d       -> AbsoluteAddress (endAddr `addOff` fromIntegral d) -- (Address (fromIntegral (d + fromIntegral endAddr)))
  where
  addOff = addressAddOffset mem

-- Needs to convert all Disp8 IP relative references to Disp32
x64SymbolizeAddresses :: (MM.MemWidth w) => MM.Memory w -> RelAddress w -> Instruction () -> Instruction (TargetAddress w)
x64SymbolizeAddresses mem insnAddr xi@(XI ii) =
  XI (ii { D.iiArgs = fmap (saveAbsoluteRipAddresses mem insnAddr xi) (D.iiArgs ii) })

saveAbsoluteRipAddresses :: (MM.MemWidth w) => MM.Memory w -> RelAddress w -> Instruction () -> AnnotatedOperand () -> AnnotatedOperand (TargetAddress w)
saveAbsoluteRipAddresses mem insnAddr i AnnotatedOperand { aoOperand = (v, ty) } =
  AnnotatedOperand { aoOperand = (I.runIdentity (mapAddrRef promoteRipDisp8 I.Identity v), ty)
                   , aoAnnotation = addrRefToAddress (ripToAbs mem insnAddr i) v
                   }

promoteRipDisp8 :: D.AddrRef -> I.Identity D.AddrRef
promoteRipDisp8 ref =
  case ref of
    D.IP_Offset_32 seg (D.Disp8 d) -> I.Identity $ D.IP_Offset_32 seg (D.Disp32 (fromIntegral d))
    D.IP_Offset_64 seg (D.Disp8 d) -> I.Identity $ D.IP_Offset_64 seg (D.Disp32 (fromIntegral d))
    _ -> I.Identity ref

x64ConcretizeAddresses :: (L.HasCallStack, MM.MemWidth w) => MM.Memory w -> RelAddress w -> Instruction (TargetAddress w) -> Instruction ()
x64ConcretizeAddresses mem insnAddr xi@(XI ii) =
  XI $ ii { D.iiArgs = fmap (fixRipRelAddresses mem insnAddr xi) (D.iiArgs ii) }

fixRipRelAddresses :: (L.HasCallStack, MM.MemWidth w)
                   => MM.Memory w
                   -> RelAddress w
                   -> Instruction (TargetAddress w)
                   -> AnnotatedOperand (TargetAddress w)
                   -> AnnotatedOperand ()
fixRipRelAddresses mem insnAddr i AnnotatedOperand { aoOperand = (v, ty)
                                                   , aoAnnotation = targetAddr
                                                   } =
  case targetAddr of
    NoAddress -> mkUnitAnnot (v, ty)
    AbsoluteAddress a
      | Just v' <- mapAddrRef (absToRip mem insnAddr i a) (const Nothing) v -> mkUnitAnnot (v', ty)
      | otherwise -> L.error "Unexpected rip rel fix"

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
absToRip :: (MM.MemWidth w)
         => MM.Memory w
         -> RelAddress w
         -> Instruction (TargetAddress w)
         -> RelAddress w
         -> D.AddrRef
         -> Maybe D.AddrRef
absToRip mem iStartAddr i a ref =
  case ref of
    D.IP_Offset_32 seg (D.Disp32 _) ->
      Just $ D.IP_Offset_32 seg (D.Disp32 (fromIntegral (a `addressDiff` iEndAddr)))
    D.IP_Offset_64 seg (D.Disp32 _) ->
      Just $ D.IP_Offset_64 seg (D.Disp32 (fromIntegral (a `addressDiff` iEndAddr)))
    _ -> Nothing
  where
    iEndAddr = iStartAddr `addOff` fromIntegral (x64Size i)
    addOff   = addressAddOffset mem

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

