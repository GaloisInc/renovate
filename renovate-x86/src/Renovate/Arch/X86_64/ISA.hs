{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | The 'ISA' for x86_64
module Renovate.Arch.X86_64.ISA (
  isa,
  assemble,
  disassemble
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Monad.Catch as C
import           Data.Bits ( bit )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Functor.Identity as I
import           Data.Int ( Int32 )
import qualified Data.List.NonEmpty as DLN
import           Data.Maybe
import           Data.Parameterized.Some
import           Data.Parameterized.NatRepr
import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Word ( Word8, Word64 )

import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Types as MT
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
  case D.assembleInstruction (toFlexInst i) of
    Nothing -> C.throwM (InstructionAssemblyFailure i)
    Just bldr -> return (LB.toStrict (B.toLazyByteString bldr))

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
    Just insns -> return (R.concreteBlock start insns onlyRepr pb)
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
                  return (reverse (fromFlexInst ii : insns))
                | otherwise ->
                  -- Otherwise, we just keep decoding
                  go (totalRead + bytesRead) nextAddr (B.drop bytesRead b) (fromFlexInst ii : insns)

-- | An implementation of the 'ISA' for x86_64.
--
-- This particular implementation uses @int 3@ for padding and encodes
-- jumps as @push ADDR ; ret@.
isa :: R.ISA X86.X86_64
isa = R.ISA
  { R.isaInstructionSize = x64Size
  , R.isaJumpType = x64JumpType
  , R.isaDefaultInstructionArchRepr = R.SomeInstructionArchRepr onlyRepr
  , R.isaMakeRelativeJumpTo = x64MakeRelativeJumpTo
  , R.isaMaxRelativeJumpSize = const (bit 31 - 1)
  , R.isaModifyJumpTarget = x64ModifyJumpTarget
  , R.isaMakePadding = x64MakePadding
  , R.isaSymbolizeAddresses = x64SymbolizeAddresses
  , R.isaConcretizeAddresses = x64ConcretizeAddresses
  , R.isaMakeSymbolicJump = x64MakeSymbolicJump
  , R.isaMakeSymbolicCall = x64MakeSymbolicCall
  , R.isaPrettyInstruction = show . PD.pretty
  , R.isaMove = x86Move
  , R.isaMoveImmediate = x86MoveImmediate
  , R.isaLoad = x86Load
  , R.isaStore = x86Store
  , R.isaStoreImmediate = x86StoreImmediate
  , R.isaAddImmediate = x86AddImmediate
  , R.isaSubtractImmediate = x86SubtractImmediate
  }

x86StackAddress :: R.InstructionArchRepr X86.X86_64 tp
                -> R.StackAddress X86.X86_64 tp
                -> (Some MT.TypeRepr)
                -> Value tp
x86StackAddress repr addr (Some tp) =
  case repr of
    OnlyRepr X86Repr -> do
      let (D.QWordReg base_reg) = toFlexValue (R.saBase addr)
      let x86_addr = D.Addr_64 D.SS
            (Just base_reg)
            Nothing
            (D.Disp32 $ D.Imm32Concrete $ fromIntegral $ R.saOffset addr)
      case tp of
        MT.BVTypeRepr w
          | Just Refl <- testEquality w (knownNat   @8)  -> Value (D.Mem8   x86_addr)
          | Just Refl <- testEquality w (knownNat  @16)  -> Value (D.Mem16  x86_addr)
          | Just Refl <- testEquality w (knownNat  @32)  -> Value (D.Mem32  x86_addr)
          | Just Refl <- testEquality w (knownNat  @64)  -> Value (D.Mem64  x86_addr)
          | Just Refl <- testEquality w (knownNat @128)  -> Value (D.Mem128 x86_addr)
        MT.FloatTypeRepr MT.SingleFloatRepr -> Value (D.Mem128 x86_addr)
        MT.FloatTypeRepr MT.DoubleFloatRepr -> Value (D.Mem128 x86_addr)
        _  -> error $ "unexpected move type: " ++ show tp

x86MovName :: Some MT.TypeRepr -> String
x86MovName (Some tp) = case tp of
  MT.BVTypeRepr{} -> "mov"
  MT.FloatTypeRepr MT.SingleFloatRepr -> "movss"
  MT.FloatTypeRepr MT.DoubleFloatRepr -> "movsd"
  _ -> error $ "unexpected move type: " ++ show tp

x86YMMToXMM :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
             . Some MT.TypeRepr
            -> Value tp
            -> Value tp
x86YMMToXMM (Some tp) val
  | MT.FloatTypeRepr{} <- tp
  , Value (D.YMMReg reg) <- val =
    Value (D.XMMReg $ D.xmmReg $ D.ymmRegNo reg)
  | otherwise =
    val

x86MakeMovInstr
  :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
   . Some MT.TypeRepr
  -> R.InstructionArchRepr X86.X86_64 tp
  -> Value tp
  -> Value tp
  -> Instruction tp TargetAddress
x86MakeMovInstr tp repr dest src =
  case repr of
    OnlyRepr X86Repr ->
      noAddr $ makeInstr (x86MovName tp) $ map (toFlexValue . x86YMMToXMM tp) [dest, src]

x86Move
  :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
   . Some MT.TypeRepr
  -> R.InstructionArchRepr X86.X86_64 tp
  -> Value tp
  -> Value tp
  -> Instruction tp TargetAddress
x86Move = x86MakeMovInstr

x86MoveImmediate
  :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
   . Some MT.TypeRepr
  -> R.InstructionArchRepr X86.X86_64 tp
  -> Value tp
  -> Integer
  -> Instruction tp TargetAddress
x86MoveImmediate tp repr dest_reg imm =
  case repr of
    OnlyRepr X86Repr ->
      x86MakeMovInstr tp repr dest_reg (Value (D.QWordImm $ D.UImm64Concrete $ fromIntegral imm))

x86Load
  :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
   . Some MT.TypeRepr
  -> InstructionArchRepr X86.X86_64 tp
  -> Value tp
  -> R.StackAddress X86.X86_64 tp
  -> Instruction tp TargetAddress
x86Load tp repr reg addr =
  case repr of
    OnlyRepr X86Repr ->
      x86MakeMovInstr tp repr reg (x86StackAddress repr addr tp)

x86Store
  :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
   . Some MT.TypeRepr
  -> R.InstructionArchRepr X86.X86_64 tp
  -> R.StackAddress X86.X86_64 tp
  -> Value tp
  -> Instruction tp TargetAddress
x86Store tp repr addr reg =
  case repr of
    OnlyRepr X86Repr ->
      x86MakeMovInstr tp repr (x86StackAddress repr addr tp) reg

x86StoreImmediate
  :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
   . Some MT.TypeRepr
  -> InstructionArchRepr X86.X86_64 tp
  -> R.StackAddress X86.X86_64 tp
  -> Integer
  -> Instruction tp TargetAddress
x86StoreImmediate tp repr addr imm =
  case repr of
    OnlyRepr X86Repr ->
      x86MakeMovInstr tp repr (x86StackAddress repr addr tp) (Value (D.QWordImm $ D.UImm64Concrete $ fromIntegral imm))

x86AddImmediate :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
                 . Value tp
                -> Integer
                -> [Instruction tp TargetAddress]
x86AddImmediate _ = x86OpSPImmediate $ B.pack [0x4c, 0x01, 0xd4]

x86SubtractImmediate :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
                      . Value tp
                     -> Integer
                     -> [Instruction tp TargetAddress]
x86SubtractImmediate _ = x86OpSPImmediate $ B.pack [0x4c, 0x29, 0xd4]

-- x86OpImmediate :: String -> Value -> Integer -> Instruction TargetAddress
-- x86OpImmediate op reg imm =
--   noAddr $ makeInstr op [reg, D.DWordImm $ D.Imm32Concrete $ fromIntegral imm]
x86OpSPImmediate :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
                  . B.ByteString
                 -> Integer
                 -> [Instruction tp TargetAddress]
x86OpSPImmediate op_bytes imm = do
  let tmp_reg = D.QWordReg D.R10
  map noAddr $
    [makeInstr "mov" [tmp_reg, D.QWordImm $ D.UImm64Concrete (fromIntegral imm)]]
    ++ (map fromFlexInst $
        mapMaybe D.disInstruction $ D.disassembleBuffer op_bytes)

-- | Simple adapter for 'x64JumpTypeRaw' with the right type to be used in an
-- ISA.
x64JumpType :: forall (tp :: R.InstructionArchReprKind X86.X86_64) t
             . Instruction tp t
            -> MM.Memory 64
            -> R.ConcreteAddress X86.X86_64
            -> Some (R.JumpType X86.X86_64)
x64JumpType insn _ = x64JumpTypeRaw insn

-- | Classify different kinds of jump instructions.
--
-- Fortunately, all of the conditional jumps start with 'j', and no
-- other instructions start with 'j'.  This lets us have an easy case
-- to handle all of them.
x64JumpTypeRaw :: forall (tp :: R.InstructionArchReprKind X86.X86_64) t
                . Instruction tp t
               -> R.ConcreteAddress X86.X86_64
               -> Some (R.JumpType X86.X86_64)
x64JumpTypeRaw xi@(XI ii) addr =
  case (D.iiOp ii, map (fst . aoOperand) (D.iiArgs ii)) of
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
x64MakeRelativeJumpTo srcAddr targetAddr _repr
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
    jmpInstr = makeInstr "jmp"
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

x64MakeSymbolicJump
  :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
   . R.SymbolicAddress X86.X86_64
  -> R.InstructionArchRepr X86.X86_64 tp
  -> [R.TaggedInstruction X86.X86_64 tp TargetAddress]
x64MakeSymbolicJump sym_addr _ = [x64MakeSymbolicJumpOrCall "jmp" sym_addr]

x64MakeSymbolicCall
  :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
   . R.SymbolicAddress X86.X86_64
  -> R.TaggedInstruction X86.X86_64 tp TargetAddress
x64MakeSymbolicCall = x64MakeSymbolicJumpOrCall "call"

x64MakeSymbolicJumpOrCall
  :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
   . String
  -> R.SymbolicAddress X86.X86_64
  -> R.TaggedInstruction X86.X86_64 tp TargetAddress
x64MakeSymbolicJumpOrCall op_code sym_addr =
  R.tagInstruction (Just sym_addr) $
    noAddr $ makeInstr op_code [D.JumpOffset D.JSize32 $ D.FixedOffset 0]

-- NOTE: This is called only on jump instructions (the 'R.JumpType' argument is
-- intended to be evidence of that).
x64ModifyJumpTarget :: R.ConcreteAddress X86.X86_64
                    -> R.Instruction X86.X86_64 tp ()
                    -> R.JumpType arch R.HasModifiableTarget
                    -> R.ConcreteAddress X86.X86_64
                    -> Maybe (DLN.NonEmpty (Instruction tp ()))
x64ModifyJumpTarget srcAddr (XI ii) _jt newTarget =
  case D.iiOp ii of
    'j' : _ -> Just (extendDirectJump newTarget DLN.:| [])
    'c' : 'a' : 'l' : 'l' : _ -> Just (extendDirectJump newTarget DLN.:| [])
    _ -> RP.panic RP.X86_64ISA "x64ModifyJumpTarget" [ "Unexpected non-jump passed to isaModifyJumpTarget " ++ show ii
                                                     ]
  where extendDirectJump targetAddr = case aoOperand <$> D.iiArgs ii of
          [(D.JumpOffset D.JSize32 _, _)] -> jmpInstr (D.iiOp ii) 0 targetAddr
          [(D.JumpOffset _ _, _)] -> L.error ("Expected a 4-byte jump offset: " ++ show ii)
          _ -> XI ii

        jmpOffsetUnsafe :: String -> Word8 -> R.ConcreteAddress X86.X86_64 -> Integer
        jmpOffsetUnsafe opcode offset targetAddr = fromIntegral $ (targetAddr `R.addressDiff` srcAddr) - fromIntegral (jmpInstrSizeGuess opcode + offset)
        i32Max :: Integer
        i32Max = fromIntegral (maxBound :: Int32)
        jmpOffset opcode offset targetAddr
          | abs off > i32Max =
            L.error ("Relative branch is out of range: from " ++ show srcAddr ++
                     " to " ++ show targetAddr)
          | otherwise = off
          where off = jmpOffsetUnsafe opcode offset targetAddr

        -- Make a dummy instruction of the correct type to get a size
        -- guess.  This should never fail the size check.  We have to
        -- generate an instruction and check its size because some jumps
        -- could be 5 bytes, while others could be 6.
        dummyJumpInstr opcode = makeInstr opcode [D.JumpOffset D.JSize32 (D.FixedOffset 0)]
        jmpInstrSizeGuess opcode = x64Size (dummyJumpInstr opcode)
        jmpInstrUnsafe opcode offset targetAddr = makeInstr opcode [D.JumpOffset D.JSize32 (D.FixedOffset (fromIntegral (jmpOffset opcode offset targetAddr)))]
        jmpInstr opcode offset targetAddr
          | jmpInstrSizeGuess opcode /= x64Size jmp =
            L.error ("BUG! The 'jmp' instruction size is not as expected! " ++
                     "Expected " ++ show (jmpInstrSizeGuess opcode) ++ " but got " ++
                     show (x64Size jmp))
          | otherwise = jmp
          where jmp = jmpInstrUnsafe opcode offset targetAddr

-- | Make @n@ bytes of @int 3@ instructions.  If executed, these
-- generate an interrupt that drops the application into a debugger
-- (or terminates it).
--
-- An alternative would be to use the two-byte undefined instruction
-- @ud2@ where possible and pad the rest with an @int 3@.
x64MakePadding :: Word64 -> R.InstructionArchRepr X86.X86_64 tp -> [Instruction tp ()]
x64MakePadding nBytes _ =
  replicate (fromIntegral nBytes) (makeInstr "int3" [])

addrRefToAddress :: (D.AddrRef -> TargetAddress) -> D.Value -> TargetAddress
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

ripToAbs :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
          . MM.Memory 64
         -> R.ConcreteAddress X86.X86_64
         -> Instruction tp ()
         -> D.AddrRef
         -> TargetAddress
ripToAbs mem iStartAddr i ref =
  case ref of
    D.IP_Offset_32 _seg disp -> absoluteDisplacement mem iEndAddr disp
    D.IP_Offset_64 _seg disp -> absoluteDisplacement mem iEndAddr disp
    _ -> NoAddress
  where
    addOff   = R.addressAddOffset
    iEndAddr = iStartAddr `addOff` fromIntegral (x64Size i)

absoluteDisplacement :: MM.Memory 64 -> R.ConcreteAddress X86.X86_64 -> D.Displacement -> TargetAddress
absoluteDisplacement _mem endAddr disp =
  case disp of
    D.NoDisplacement -> L.error "Unexpected NoDisplacement"
    D.Disp8  d       -> AbsoluteAddress (endAddr `R.addressAddOffset` fromIntegral d)
    D.Disp32 (D.Imm32Concrete d)       -> AbsoluteAddress (endAddr `R.addressAddOffset` fromIntegral d)
    D.Disp32 (D.Imm32SymbolOffset {}) -> L.error "Symbolic references are not supported"

-- | Needs to convert all Disp8 IP relative references to Disp32
--
-- NOTE: The jump instructions don't have any IP-relative memory address
-- references, so we don't need to update any IP-relative address operands.
-- However, we do need to extend any jumps we find into jumps with 4 byte
-- offsets.
x64SymbolizeAddresses :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
                       . MM.Memory 64
                      -> R.ConcreteAddress X86.X86_64
                      -> Maybe (R.SymbolicAddress X86.X86_64)
                      -> Instruction tp ()
                      -> [R.TaggedInstruction X86.X86_64 tp TargetAddress]
x64SymbolizeAddresses mem insnAddr mSymbolicTarget xi@(XI ii)
  | 'j' : _ <- D.iiOp ii = [R.tagInstruction mSymbolicTarget jmpInstr]
  | 'c' : 'a' : 'l' : 'l' : _ <- D.iiOp ii
  , [ AnnotatedOperand { aoOperand = (D.JumpOffset {}, _) } ] <- D.iiArgs ii
  = [R.tagInstruction mSymbolicTarget jmpInstr]
  | otherwise = [R.tagInstruction mSymbolicTarget newInsn]
  where
    newInsn = XI (ii { D.iiArgs = fmap (saveAbsoluteRipAddresses mem insnAddr xi) (D.iiArgs ii) })
    XI jmpInstr0 = makeInstr (D.iiOp ii) [D.JumpOffset D.JSize32 (D.FixedOffset 0)]
    jmpInstr = XI (jmpInstr0 { D.iiArgs = fmap (saveAbsoluteRipAddresses mem insnAddr xi) (D.iiArgs jmpInstr0) })


saveAbsoluteRipAddresses :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
                          . MM.Memory 64
                         -> R.ConcreteAddress X86.X86_64
                         -> Instruction tp ()
                         -> AnnotatedOperand ()
                         -> AnnotatedOperand TargetAddress
saveAbsoluteRipAddresses mem insnAddr i AnnotatedOperand { aoOperand = (v, ty) } =
  AnnotatedOperand { aoOperand = (I.runIdentity (mapAddrRef promoteRipDisp8 I.Identity v), ty)
                   , aoAnnotation = addrRefToAddress (ripToAbs mem insnAddr i) v
                   }

promoteRipDisp8 :: D.AddrRef -> I.Identity D.AddrRef
promoteRipDisp8 ref =
  let toD32 = D.Disp32 . D.Imm32Concrete . fromIntegral in
  case ref of
    D.IP_Offset_32 seg (D.Disp8 d) -> I.Identity $ D.IP_Offset_32 seg $ toD32 d
    D.IP_Offset_64 seg (D.Disp8 d) -> I.Identity $ D.IP_Offset_64 seg $ toD32 d
    _ -> I.Identity ref

x64ConcretizeAddresses :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
                        . (L.HasCallStack)
                       => MM.Memory 64
                       -> R.ConcreteAddress X86.X86_64
                       -> Instruction tp TargetAddress
                       -> Instruction tp ()
x64ConcretizeAddresses mem insnAddr xi@(XI ii) =
  XI $ ii { D.iiArgs = fmap (fixRipRelAddresses mem insnAddr xi) (D.iiArgs ii) }

fixRipRelAddresses :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
                    . (L.HasCallStack)
                   => MM.Memory 64
                   -> R.ConcreteAddress X86.X86_64
                   -> Instruction tp TargetAddress
                   -> AnnotatedOperand TargetAddress
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
absToRip :: forall (tp :: R.InstructionArchReprKind X86.X86_64)
          . MM.Memory 64
         -> R.ConcreteAddress X86.X86_64
         -> Instruction tp TargetAddress
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

