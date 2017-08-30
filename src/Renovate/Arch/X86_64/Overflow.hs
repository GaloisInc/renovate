{-# LANGUAGE ViewPatterns #-}
-- | The integer overflow analysis for x86_64
module SFE.Arch.X86_64.Overflow ( overflowAnalysis ) where

import qualified Data.Set as S

import qualified Flexdis86 as D

import SFE.Arch.X86_64.Internal
import SFE.Analysis.Overflow

-- | The overflow analysis for x86_64
overflowAnalysis :: OverflowAnalysis Instruction Value
overflowAnalysis =
  OverflowAnalysis { oaTaintsRegister = x64TaintsRegister
                   , oaPropagatesTaint = x64PropagatesTaint
                   , oaTaintSinks = x64TaintSinks
                   }

-- | Look at an instruction and, if the instruction is arithmetic,
-- return the value (register?) that is "tainted" (modified).
--
-- For unary operations (e.g., inc), the operand is also the
-- destination.
--
-- For binary operations like add/sub, it looks like the lhs operand
-- is always the one modified.  mul will be different.
--
-- Question: Do we want to be able to represent both registers and
-- memory?  Just registers for now, so maybe don't complicate things...
x64TaintsRegister :: Instruction a -> Maybe Value
x64TaintsRegister ii
  | isArithmeticInstruction ii =
    case instrOperands ii of
      [(val, _)] -> Just val
      [(v1, _v1t), _] -> Just v1
      _ -> Nothing
  | otherwise = Nothing

-- | Registers can be sanitized if they are overwritten with a new
-- value (e.g., mov) or capped by a mod.
--
-- 'mov' clobbers the lhs and propagates taint from the rhs to the lhs
--
-- Taint is also propagated through arithmetic instructions
x64PropagatesTaint :: Instruction a -> [(Value, Maybe Value)]
x64PropagatesTaint ii =
  case (instrOpcode ii, instrOperands ii) of
    (_, [(v,_)]) -> [ (v, Just v) ]
    (_, [(lhs,_), (rhs, _)]) -> [ (rhs, Just lhs)
                                ]
    _ -> []

-- | We probably want to say that a tainted int used in a @lea@ or a
-- @mov@ to a memory address with a SIB (i.e., where a reg is used as
-- a scaled base/index)
x64TaintSinks :: Instruction a -> [Value]
x64TaintSinks ii =
  case (instrOpcode ii, instrOperands ii) of
    ("lea", [_, (memoryReferenceIndex -> Just reg, _)]) -> [reg]
    (_, [(memoryReferenceIndex -> Just reg, _), _]) -> [reg]
    (_, [_, (memoryReferenceIndex -> Just reg, _)]) -> [reg]
    _ -> []

-- | If the input 'D.Value' is a memory reference, return its index
-- register (if any).
--
-- In an instruction like
--
-- > mov rax,QWORD PTR [rsi+rdi*8]
--
-- the second operand is a memory reference.  Its index register is
-- rdi (it is multiplied by a scalar factor).  This is often used as
-- an array access.
--
-- Note, this doesn't handle array references through strength
-- reduction; that will have to be a separate check.
memoryReferenceIndex :: Value -> Maybe Value
memoryReferenceIndex v = do
  aref <- valueMemoryReference v
  case aref of
    D.Addr_32 _ _ (Just (_, r32)) _ -> return (D.DWordReg r32)
    D.Addr_64 _ _ (Just (_, r64)) _ -> return (D.QWordReg r64)
    _ -> Nothing

valueMemoryReference :: Value -> Maybe D.AddrRef
valueMemoryReference v =
  case v of
    D.FarPointer m -> Just m
    D.VoidMem m -> Just m
    D.Mem8 m -> Just m
    D.Mem16 m -> Just m
    D.Mem32 m -> Just m
    D.Mem64 m -> Just m
    D.FPMem32 m -> Just m
    D.FPMem64 m -> Just m
    D.FPMem80 m -> Just m
    _ -> Nothing

isArithmeticInstruction :: Instruction a -> Bool
isArithmeticInstruction ii = instrOpcode ii `S.member` arithInsns

arithInsns :: S.Set String
arithInsns = S.fromList [ "add"
                        , "sub"
                        , "inc"
                        , "dec"
                        ]

{-

To really do this analysis, we need to be able to track tainted values
through registers.  After the initial taint, the tainted value could
be moved around without the taint being cleared, then used.

We need three functions:

* introduceTaint :: i -> Maybe r
* propagatesTaint :: i -> [(r, Maybe r)]
* taintSink :: i -> r -> Bool

propagatesTaint says that the taint in the first reg was propagated to
the second (in each pair).  If the second in a pair is Nothing, that
would indicate that the instruction cleared taint from the given
register.

The taintSink function returns True if the given register r would be
problematic in i if it was tainted.

Note that we want multiple registers at each position, as x86
registers alias each other at different bit sizes.  For now, we can
just normalize to the widest register available.

-}

