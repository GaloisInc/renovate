{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Internal helpers for the x86_64 ISA implementation
module Renovate.Arch.X86_64.Internal (
  instructionRepr,
  valueRepr,
  makeInstr,
  toFlexInst,
  fromFlexInst,
  Instruction(..),
  AnnotatedOperand(..),
  TargetAddress(..),
  Value(..),
  toFlexValue,
  instrOpcode,
  instrOperands,
  mkUnitAnnot,
  annotateInstr,
  annotateInstrWith,
  noAddr,
  x64Size,
  prettyPrintWithAnnotations,
  OnlyEncoding,
  X86Repr(..),
  -- * Errors
  AssemblyFailure(..),
  DisassemblyFailure(..)
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe ( fromMaybe )
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Typeable ( Typeable )
import           Data.Word ( Word8 )
import           Numeric ( showHex )
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Data.Macaw.X86 as X86
import qualified Flexdis86 as D

import qualified Renovate as R

-- | The type of operands to x86_64 instructions
data Value tp where
  Value :: D.Value -> Value OnlyEncoding

instance Eq (Value tp) where
  Value v1 == Value v2 = v1 == v2

instance Ord (Value tp) where
  compare (Value v1) (Value v2) = compare v1 v2

instance TestEquality Value where
  testEquality (Value v1) (Value v2)
    | v1 == v2 = Just Refl
    | otherwise = Nothing

instance OrdF Value where
  compareF (Value v1) (Value v2) = fromOrdering (compare v1 v2)

toFlexValue :: Value tp -> D.Value
toFlexValue (Value v) = v

valueRepr :: Value tp -> X86Repr tp
valueRepr (Value _) = X86Repr

type instance R.RegisterType X86.X86_64 = Value

-- | The target address of a jump.  This is used as the annotation
-- type for symbolic instructions.
data TargetAddress = NoAddress
                   | AbsoluteAddress (R.ConcreteAddress X86.X86_64)
                   deriving (Eq, Ord, Show)

-- | The type of an operand with an annotation
data AnnotatedOperand a = AnnotatedOperand { aoOperand :: (D.Value, D.OperandType)
                                           , aoAnnotation :: a
                                           }
                        deriving (Functor, Eq, Show)

data EncodingKind = OnlyEncoding
type OnlyEncoding = 'OnlyEncoding

type instance R.InstructionArchReprKind X86.X86_64 = EncodingKind
type instance R.InstructionArchRepr X86.X86_64 = X86Repr

data X86Repr tp where
  X86Repr :: X86Repr OnlyEncoding

instance TestEquality X86Repr where
  testEquality X86Repr X86Repr = Just Refl

instance OrdF X86Repr where
  compareF X86Repr X86Repr = EQF

-- | A wrapper around a flexdis86 instruction with an arbitrary
-- annotation on each operand of type @a@.
data Instruction (tp :: R.InstructionArchReprKind X86.X86_64) a where
  XI :: D.InstructionInstanceF (AnnotatedOperand a) -> Instruction OnlyEncoding a
  RawBytes :: B.ByteString -> Instruction OnlyEncoding a

instructionRepr :: Instruction tp a -> X86Repr tp
instructionRepr i =
  case i of
    XI {} -> X86Repr
    RawBytes {} -> X86Repr

deriving instance (Show a) => Show (Instruction tp a)
deriving instance (Eq a) => Eq (Instruction tp a)
deriving instance Functor (Instruction tp)

type instance R.Instruction X86.X86_64 = Instruction
type instance R.InstructionAnnotation X86.X86_64 = TargetAddress

instance PD.Pretty (Instruction tp a) where
  pretty = PD.pretty . prettyPrint

-- Note that the base is incorrect, so the display won't be perfect.
-- We can't really get the address here, so we'll have to come up with
-- something else longer term.
prettyPrint :: Instruction tp a -> String
prettyPrint = prettyPrint' (doPP Nothing)

doPP :: Maybe (AnnotatedOperand a -> PP.Doc) -> Instruction tp a -> PP.Doc
doPP Nothing (XI ii) = D.ppInstruction (fmap aoOperand ii)
doPP (Just ppWith) (XI ii) = D.ppInstructionWith ppWith ii
doPP _ (RawBytes b) = PP.pretty "data: " <> PP.hsep [PP.pretty (showHex w "") | w <- B.unpack b]

prettyPrint' :: (Instruction tp a -> PP.Doc) -> Instruction tp a -> String
prettyPrint' pp i = PP.displayS (PP.renderCompact (pp i)) ""

prettyPrintWithAnnotations :: R.TaggedInstruction X86.X86_64 tp TargetAddress
                           -> String
prettyPrintWithAnnotations insn =
  prettyPrint' (doPP (Just ppValue)) (R.projectInstruction insn) ++ insnAnnStr
  where
    ppValue ao = PP.hsep $ D.ppValue (fst $ aoOperand ao) : annDocs
      where
        annDocs = case aoAnnotation ao of
          NoAddress -> []
          AbsoluteAddress addr -> [PP.angles (PP.text $ show $ PD.pretty addr)]

    insnAnnStr = case R.symbolicTarget insn of
      Some (R.RelocatableTarget tgt) -> " <<" ++ show (PD.pretty tgt) ++ ">>"
      Some R.NoTarget -> ""

-- | The types of failures that can occur during disassembly of x86_64
-- instructions.
--
-- Failures could be:
--
-- 1) Invalid code
--
-- 2) Bugs in flexdis
--
-- 3) Bugs in the code discovery code
data DisassemblyFailure =
  InstructionDisassemblyFailure Int Int
  -- ^ The disassembler was unable to disassemble at the given
  -- location.  The first 'Int' is the byte offset at which
  -- disassembly failed.  The second 'Int' is the number of bytes
  -- consumed before disassembly failed.
  | InstructionDisassemblyFailure1 B.ByteString Int
  -- ^ The byte string being parsed and the number of bytes consumed
  -- before the parser gave up
  | OverlappingBlocks (R.ConcreteAddress X86.X86_64) (R.ConcreteAddress X86.X86_64) (R.ConcreteAddress X86.X86_64)
  | EmptyBlock (R.ConcreteAddress X86.X86_64)
  deriving (Show, Typeable)

instance C.Exception DisassemblyFailure

-- | An exception thrown when an instruction cannot be assembled (due
-- to malformed operands or a bug in the assembler)
data AssemblyFailure = forall tp . InstructionAssemblyFailure (Instruction tp ())

instance Show AssemblyFailure where
  show af =
    case af of
      InstructionAssemblyFailure i -> show i

instance C.Exception AssemblyFailure

-- | Retrieve the 'String' opcode from an instruction (for diagnostic purposes)
instrOpcode :: Instruction tp a -> String
instrOpcode (XI ii) = D.iiOp ii
instrOpcode (RawBytes _) = "<data>"

-- | Extract the operands from an instruction
instrOperands :: Instruction tp a -> [(D.Value, D.OperandType)]
instrOperands (XI ii) = fmap aoOperand (D.iiArgs ii)
instrOperands (RawBytes _) = []

-- | Strip off our wrapper around a flexdis86 instruction.
--
-- We end up needing this to assemble an instruction and pretty print.
toFlexInst :: Instruction tp a -> Maybe D.InstructionInstance
toFlexInst (XI ii) = Just (fmap aoOperand ii)
toFlexInst (RawBytes _) = Nothing

-- | Wrap a flexdis86 instruction with our wrapper that fixes some
-- operand types and hides the flexdis type from the rest of the code.
fromFlexInst :: X86Repr tp -> D.InstructionInstance -> Instruction tp ()
fromFlexInst X86Repr ii = XI (fmap mkUnitAnnot ii)

-- | Make a new annotated operand with a trivial (unit) annotation
mkUnitAnnot :: (D.Value, D.OperandType) -> AnnotatedOperand ()
mkUnitAnnot o = AnnotatedOperand { aoOperand = o, aoAnnotation = () }

-- | Helper function for making an x86 instruction.
makeInstr :: X86Repr tp -> String -> [D.Value] -> Instruction tp ()
makeInstr X86Repr mnemonic operands =
  case D.mkInstruction mnemonic operands of
    Just i -> fromFlexInst X86Repr i
    Nothing -> L.error ("Renovate.ISA.X64: Could not create an instruction for '" ++ mnemonic ++ " " ++ show operands ++ "'")

annotateInstr :: Instruction tp () -> a -> Instruction tp a
annotateInstr (XI ii) a = XI (fmap (\ao -> ao { aoAnnotation = a}) ii)
annotateInstr (RawBytes b) _ = RawBytes b

annotateInstrWith :: (AnnotatedOperand () -> AnnotatedOperand a) -> Instruction tp () -> Instruction tp a
annotateInstrWith f (XI ii) = XI (fmap f ii)
annotateInstrWith _f (RawBytes b) = RawBytes b

noAddr :: Instruction tp () -> Instruction tp TargetAddress
noAddr i = annotateInstr i NoAddress

-- | Compute the size of an instruction by assembling it and
-- measuring.
--
-- It might be faster to just inspect the encoding; we can do that
-- later.
--
-- FIXME: This needs to be bigger than word8 if we can embed data
x64Size :: Instruction tp t -> Word8
x64Size i =
  case i of
    XI ii ->
      let err = error ("Invalid instruction")
          b = BB.toLazyByteString $ fromMaybe err $ D.assembleInstruction (fmap aoOperand ii)
      in fromIntegral (LB.length b)
    RawBytes b -> fromIntegral (B.length b)
