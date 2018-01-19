{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Renovate.Arch.PPC.ISA (
  isa,
  assemble,
  disassemble,
  Instruction,
  TargetAddress(..),
  toInst,
  fromInst,
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
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Word ( Word8, Word64 )
import           Text.Printf ( printf )

import qualified Data.Macaw.Memory as MM
import           Data.Parameterized.Classes ( showF )
import qualified Data.Parameterized.TraversableFC as FC
import qualified Dismantle.PPC as D

import           Renovate

data TargetAddress w = NoAddress
                     | AbsoluteAddress (ConcreteAddress w)
                     deriving (Eq, Ord, Show)

newtype Instruction a = I { unI :: D.AnnotatedInstruction a }
  deriving (Show)

instance PP.Pretty (Instruction a) where
  pretty = PP.pretty . ppcPrettyInstruction

instance Functor Instruction where
  fmap f (I i) =
    case i of
      D.Instruction opc operands ->
        I (D.Instruction (coerce opc) (FC.fmapFC (\(D.Annotated a o) -> D.Annotated (f a) o) operands))

assemble :: (C.MonadThrow m) => Instruction () -> m B.ByteString
assemble = return . LB.toStrict . D.assembleInstruction . toInst

disassemble :: (C.MonadThrow m) => B.ByteString -> m (Int, Instruction ())
disassemble bs =
  case minsn of
    Just i -> return (bytesConsumed, fromInst i)
    Nothing -> C.throwM (InstructionDisassemblyFailure bs bytesConsumed)
  where
    (bytesConsumed, minsn) = D.disassembleInstruction (LB.fromStrict bs)

data InstructionDisassemblyFailure =
  InstructionDisassemblyFailure B.ByteString Int
  deriving (Show)

instance C.Exception InstructionDisassemblyFailure

-- | An 'ISA' description for PowerPC
--
-- For now, the same description works for both PowerPC 32 and PowerPC 64.
isa :: (MM.MemWidth w) => ISA Instruction (TargetAddress w) w
isa =
  ISA { isaInstructionSize = ppcInstrSize
      , isaPrettyInstruction = ppcPrettyInstruction
      , isaMakePadding = ppcMakePadding
      , isaMakeRelativeJumpTo = ppcMakeRelativeJumpTo
      , isaJumpType = ppcJumpType
      , isaModifyJumpTarget = ppcModifyJumpTarget
      , isaMakeSymbolicJump = ppcMakeSymbolicJump
      , isaConcretizeAddresses = ppcConcretizeAddresses
      , isaSymbolizeAddresses = ppcSymbolizeAddresses
      , isaMakeTrapIf = error "makeTrapIf is no implemented for PowerPC yet -- move this function out of ISA"
      }

ppcPrettyInstruction :: Instruction a -> String
ppcPrettyInstruction = show . D.ppInstruction . toInst

-- | All instructions on PowerPC are 4 bytes
ppcInstrSize :: Instruction a -> Word8
ppcInstrSize _ = 4

-- | Make the requested number of bytes of padding instructions (as TRAP
-- instructions).  We can only support numbers of bytes that are multiples of
-- four, as that is the only instruction size on PowerPC.
ppcMakePadding :: (HasCallStack) => Word64 -> [Instruction ()]
ppcMakePadding nBytes
  | leftover == 0 = replicate nInsns (fromInst nopInsn)
  | otherwise = error (printf "Unexpected byte count (%d); PowerPC only supports instruction-sized padding" nBytes)
  where
    (nInsns, leftover) = fromIntegral nBytes `divMod` 4
    nopInsn = D.Instruction D.TRAP D.Nil

-- | Make an unconditional relative jump from the given @srcAddr@ to the
-- @targetAddr@.
ppcMakeRelativeJumpTo :: (MM.MemWidth w) => ConcreteAddress w -> ConcreteAddress w -> [Instruction ()]
ppcMakeRelativeJumpTo srcAddr targetAddr
  | offset `mod` 4 /= 0 =
    error (printf "Unaligned jump with source=%s and target=%s" (show srcAddr) (show targetAddr))
  | offset >= 2 ^ (25 :: Int) =
    error (printf "Jump target is too far away with source=%s and target=%s" (show srcAddr) (show targetAddr))
  | otherwise = [fromInst jumpInstr]
  where
    -- We are limited to 24 + 2 bits of offset, where the low two bits must be zero.
    offset :: Integer
    offset = fromIntegral (targetAddr `addressDiff` srcAddr)

    -- We checked to make sure the low bits are zero with the mod case above.
    -- Now we shift off two of the required zeros.
    shiftedOffset :: Int32
    shiftedOffset = fromIntegral offset `shiftR` 2
    jumpInstr = D.Instruction D.B (D.Directbrtarget (D.BT shiftedOffset) D.:< D.Nil)

ppcMakeSymbolicJump :: (MM.MemWidth w) => SymbolicAddress -> [TaggedInstruction Instruction (TargetAddress w)]
ppcMakeSymbolicJump symAddr = [tagInstruction (Just symAddr) i]
  where
    -- The jump has an invalid destination because it is just a stand-in; it
    -- will be rewritten with a real jump target when we concretize the
    -- instruction.
    jmp = D.Instruction D.B (D.Directbrtarget (D.BT 0) D.:< D.Nil)
    i = annotateInstr (fromInst jmp) NoAddress

-- | This function converts symbolic address references in operands back to
-- concrete values.  As with 'ppcSymbolizeAddresses', it is a no-op on PowerPC.
ppcConcretizeAddresses :: (MM.MemWidth w) => MM.Memory w -> ConcreteAddress w -> Instruction (TargetAddress w) -> Instruction ()
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
ppcSymbolizeAddresses :: (MM.MemWidth w) => MM.Memory w -> ConcreteAddress w -> Instruction () -> Instruction (TargetAddress w)
ppcSymbolizeAddresses _mem _addr i =
  case unI i of
    D.Instruction opc operands ->
      I (D.Instruction (coerce opc) (FC.fmapFC (\(D.Annotated _ operand) -> D.Annotated NoAddress operand) operands))

-- | Classify jumps (and determine their targets, where possible)
ppcJumpType :: (HasCallStack, MM.MemWidth w) => Instruction t -> MM.Memory w -> ConcreteAddress w -> JumpType w
ppcJumpType i _mem insnAddr =
  case toInst i of
    D.Instruction opc operands ->
      case operands of
        D.Directbrtarget (D.BT offset) D.:< D.Nil ->
          RelativeJump Unconditional insnAddr (fromIntegral (offset `shiftL` 2))
        -- GBC has an extra argument generalizing to include a branch hint
        D.Condbrtarget (D.CBT offset) D.:< _crbit D.:< _bh D.:< D.Nil ->
          RelativeJump Conditional insnAddr (fromIntegral (offset `shiftL` 2))
        D.Condbrtarget (D.CBT offset) D.:< _crbit D.:< D.Nil ->
          RelativeJump Conditional insnAddr (fromIntegral (offset `shiftL` 2))
        D.Condbrtarget (D.CBT offset) D.:< D.Nil ->
          case opc of
            D.BCLalways ->
              RelativeJump Unconditional insnAddr (fromIntegral (offset `shiftL` 2))
            _ ->
              RelativeJump Conditional insnAddr (fromIntegral (offset `shiftL` 2))
        D.Absdirectbrtarget _ D.:< D.Nil ->
          error ("Absolute jumps are not supported: " ++ showF opc)
        D.Abscondbrtarget _ D.:< D.Nil ->
          error ("Absolute jumps are not supported: " ++ showF opc)
        D.Abscondbrtarget _ D.:< _ D.:< _ D.:< D.Nil ->
          error ("Absolute jumps are not supported: " ++ showF opc)
        D.Nil ->
          case opc of
            D.BCTR -> IndirectJump Unconditional
            D.BCTRL -> IndirectCall
            D.TRAP -> IndirectCall
            -- Conditional branches to link register
            D.BDNZLR -> IndirectCall    -- Some kind of conditional return
            D.BDNZLRL -> IndirectCall   -- Conditional return and link
            D.BDNZLRLm -> IndirectCall
            D.BDNZLRLp -> IndirectCall
            D.BDNZLRm -> IndirectCall
            D.BDNZLRp -> IndirectCall
            D.BDZLR -> IndirectCall
            D.BDZLRL -> IndirectCall
            D.BDZLRLm -> IndirectCall
            D.BDZLRLp -> IndirectCall
            D.BDZLRm -> IndirectCall
            D.BDZLRp -> IndirectCall
            -- Normal return (branch to link register)
            D.BLR -> Return
            D.BLRL -> Return
            _ -> NoJump
        (_ D.:< _) ->
          -- In this case, we handle all of the branches that don't need to inspect
          -- operands (because they are indirect)
          case opc of
            -- Conditional branch through the CTR register
            D.BCCTR -> IndirectJump Conditional
            D.GBCCTR -> IndirectJump Conditional
            -- This is a call because it is setting the link register and could
            -- return to the next instruction
            D.BCCTRL -> IndirectCall
            D.BCL -> IndirectCall
            D.GBCL -> IndirectCall
            D.GBCCTRL -> IndirectCall
            -- Syscall
            D.SC -> IndirectCall
            -- Traps
            D.TW -> IndirectCall
            D.TWI -> IndirectCall
            D.TD -> IndirectCall
            D.TDI -> IndirectCall
            -- Returns with extra operands
            D.GBCLR -> Return
            D.GBCLRL -> Return
            _ -> NoJump

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
ppcModifyJumpTarget :: (HasCallStack, MM.MemWidth w)
                    => Instruction ()
                    -- ^ The instruction to modify
                    -> ConcreteAddress w
                    -- ^ The address of the instruction
                    -> ConcreteAddress w
                    -- ^ The new target address
                    -> Maybe [Instruction ()]
ppcModifyJumpTarget i srcAddr targetAddr =
  case unI i of
    D.Instruction opc operands ->
      case operands of
        D.Annotated a (D.Directbrtarget (D.BT _offset)) D.:< D.Nil ->
          Just [I (D.Instruction opc (D.Annotated a (D.Directbrtarget (D.BT (newJumpOffset 26 srcAddr targetAddr `shiftR` 2))) D.:< D.Nil))]
        D.Annotated a (D.Condbrtarget (D.CBT _offset)) D.:< cond D.:< D.Nil ->
          Just [I (D.Instruction opc (D.Annotated a (D.Condbrtarget (D.CBT (newJumpOffset 16 srcAddr targetAddr `shiftR` 2))) D.:< cond D.:< D.Nil))]
        D.Annotated a (D.Condbrtarget (D.CBT _offset)) D.:< D.Nil ->
          Just [I (D.Instruction opc (D.Annotated a (D.Condbrtarget (D.CBT (newJumpOffset 16 srcAddr targetAddr `shiftR` 2))) D.:< D.Nil))]
        _ -> error ("Unexpected jump: " ++ ppcPrettyInstruction i)

-- | Compute a new jump offset between the @srcAddr@ and @targetAddr@.
--
-- If the new offset exceeds what is reachable with a single branch instruction,
-- call error.  The limit of the branch is specified as @nBits@, which is the
-- number of bits in the immediate field that will hold the offset.  Note that
-- offsets are signed, so the range check has to account for that.
newJumpOffset :: (HasCallStack, MM.MemWidth w) => Int -> ConcreteAddress w -> ConcreteAddress w -> Int32
newJumpOffset nBits srcAddr targetAddr
  | rawOff `mod` 4 /= 0 =
    error (printf "Invalid alignment for offset between src=%s and target=%s" (show srcAddr) (show targetAddr))
  | rawOff >= 2^(nBits - 1) || rawOff <= negate (2^(nBits - 1)) =
    error (printf "Jump offset too large between src=%s and target=%s" (show srcAddr) (show targetAddr))
  | otherwise = fromIntegral rawOff
  where
    rawOff = targetAddr `addressDiff` srcAddr

-- | Convert the 'Instruction' wrapper to the base instruction type, dropping
-- annotations.
--
-- Note that the coercion of the opcode is safe, because the second type
-- parameter is phantom.
toInst :: Instruction a -> D.Instruction
toInst i =
  case unI i of
    D.Instruction opc annotatedOps ->
      D.Instruction (coerce opc) (FC.fmapFC unannotateOpcode annotatedOps)

-- | Convert the base instruction type to the wrapped 'Instruction' with a unit
-- annotation.
fromInst :: D.Instruction -> Instruction ()
fromInst i =
  case i of
    D.Instruction opc unannotatedOps ->
      I (D.Instruction (coerce opc) (FC.fmapFC (D.Annotated ()) unannotatedOps))

unannotateOpcode :: D.Annotated a D.Operand tp -> D.Operand tp
unannotateOpcode (D.Annotated _ op) = op

annotateInstr :: Instruction () -> a -> Instruction a
annotateInstr (I i) a =
  case i of
    D.Instruction opc operands ->
      I (D.Instruction (coerce opc) (FC.fmapFC (\(D.Annotated _ op) -> D.Annotated a op) operands))

