{-# LANGUAGE TupleSections #-}
-- | An instrumentor that traps when overflowed arithmetic values are
-- used without being checked.
module SFE.Instrument.TrapArithOverflows (
  trapArithOverflows
  ) where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import SFE.Analysis.Overflow
import SFE.BasicBlock
import SFE.Instrument.Monad
import SFE.ABI
import SFE.ISA

-- | An instrumentor function that traps arith overflows.
--
-- TODO(conathan): extend this to only trap sensitive arith
-- overflows. To start I will trap all arith overflows.
--
-- Notes:
--
-- - Do overflow flags only get set when a full machine word -- by
--   which I mean 32-bit on x86_32 and 64-bit on x86_64 -- overflow
--   happens, or when a destination size overflow happens. E.g., if I
--   do an add into %al, will adding 1 to 127 cause OF to be set, and
--   adding 1 to 255 cause CF to bet set?
--
--   Answer: things work the way we want. Overflow flags -- both
--   signed and unsigned -- get set depending on the operand
--   size. E.g. @add %al, $1@ when @%al@ contains 127 causes an signed
--   overflow.
--
-- - How to check for overflow? There is both unsigned and signed
--   overflow. The unsigned overflow corresponds to the carry flag CF
--   being set, and the signed overflow corresponds to the overflow
--   flag OF being set. However, when analyzing the assembly, do we
--   have anyway to tell if the arith is intended to be signed or
--   unsigned?  See
--
--   - "Interrupt 4—Overflow Exception (#OF)" in Intel Manual 6-24
--     Vol. 3A: using 'into' instruction to trigger #OF exception. Or
--     not, it's not supported in 64-bit mode.
--
--   - "A.1 EFLAGS AND INSTRUCTIONS" Intel Manual Vol. 1 A-1: table
--     summarizing how instructions interact with flags. Based on the
--     table, we may be interested in at least the following
--     instructions: add, imul, inc, mul, neg, sub, xadd.
--
--   - "ADD-add" Intel Manual Vol. 2A 3-29: summary of 'add'
--     instructions and mention that 'add' does both signed and
--     unsigned arith simultaneously.
--
--   - "3.4.3.1 Status Flags" Intel Manual Vol. 1 3-15: summary of
--     flags.
--
-- - What to do on overflow? We can use 'jno' or 'jnc' or similar to
--   conditionally run code on signed and unsigned overflows,
--   respectively. Obvious choices for the trap code are 'hlt' and
--   'int3'.
--
--   I thought we could also use the 'into' instruction -- interrupt
--   on overflow -- to generate an overflow trap when the OF flag is
--   set. However, it's only supported in 32-bit mode. Irrelevant now,
--   but I also don't know if there is an 'into' analog for the CF
--   flag here. See "Interrupt 4—Overflow Exception (#OF)" in Intel
--   Manual 6-24 Vol. 3A.
--
-- - How to only trap sensitive overflows? We want to track data flow
--   in instructions, so e.g. define @writes@ and @reads@ of type
--   @D.InstructionInstance -> [D.Value]@. We can then track data flow
--   from arithmetic writes to sensitive reads.
trapArithOverflows :: (Ord v)
                   => ABI i a r w
                   -> ISA i a w
                   -> OverflowAnalysis i v
                   -> SymbolicBlock i a w
                   -> Instrument i w [TaggedInstruction i a]
trapArithOverflows abi isa oa b = concat <$> mapM insertTraps insns
  where
    insns = zip (map II [0..]) (basicBlockInstructions b)
    untaggedInsns = [ (ix, projectInstruction insn)
                    | (ix, insn) <- insns
                    ]

    trapInstrs = instructionsToTrap oa untaggedInsns

    insertTraps (ix, i)
      | ix `S.member` trapInstrs = do
          let instr = projectInstruction i
          let signedCheck = isaMakeTrapIf isa instr SignedOverflow
              unsignedCheck = isaMakeTrapIf isa instr UnsignedOverflow
          recordInstrumentation instrumentationClass (basicBlockAddress b) (fromIntegral (unwrapIndex ix))
          return (i : map (tagInstruction Nothing) (signedCheck ++ unsignedCheck))
      | otherwise = return [i]

instrumentationClass :: String
instrumentationClass = "TrapArithOverflows"

newtype InstructionIndex = II { unwrapIndex :: Int }
                         deriving (Eq, Ord, Show)

instructionsToTrap :: (Ord v) => OverflowAnalysis i v -> [(InstructionIndex, i a)] -> S.Set InstructionIndex
instructionsToTrap oa insns = odUsedWhileTainted res
  where
    res = F.foldl' (overflowTransfer oa) emptyOverflowDomain insns

data OverflowDomain r =
  OverflowDomain { odOverflowSources :: !(M.Map InstructionIndex r)
                 -- ^ The instruction index that a register was tainted at.
                 -- We track overflows by this index.
                 , odTainted :: !(M.Map r InstructionIndex)
                 -- ^ The taint index currently held by each register
                 , odUsedWhileTainted :: !(S.Set InstructionIndex)
                 -- ^ The set of values that were used in a bad way
                 -- while tainted.
                 }

overflowTransfer :: (Ord v)
                 => OverflowAnalysis i v
                 -> OverflowDomain v
                 -> (InstructionIndex, i a)
                 -> OverflowDomain v
overflowTransfer oa od (ix, i) =
  od { odUsedWhileTainted = F.foldl' (addTaintedSink od) (odUsedWhileTainted od) (oaTaintSinks oa i)
     , odOverflowSources = maybe (odOverflowSources od) (addOverflowSource od ix) newTaint
     , odTainted =
       let t1 = F.foldl' remapTaintSources (odTainted od) (oaPropagatesTaint oa i)
       in maybe t1 (addNewTaintMapping t1 ix) newTaint
     }
  where
    newTaint = oaTaintsRegister oa i

addNewTaintMapping :: (Ord v) => M.Map v InstructionIndex -> InstructionIndex -> v -> M.Map v InstructionIndex
addNewTaintMapping m ix v = M.insert v ix m

remapTaintSources :: (Ord v) => M.Map v InstructionIndex -> (v, Maybe v) -> M.Map v InstructionIndex
remapTaintSources m (src, mdst) =
  case mdst of
    -- Instruction kills taint of src
    Nothing -> M.delete src m
    -- Instruction moves the index pointed to by src to dst (but src
    -- is still tainted)
    Just dst ->
      case M.lookup src m of
        Nothing -> m
        Just taintSrc -> M.insert dst taintSrc m

addOverflowSource :: OverflowDomain v -> InstructionIndex -> v -> M.Map InstructionIndex v
addOverflowSource od ix v = M.insert ix v (odOverflowSources od)

-- | If the given value @v@ is tainted, add the source instruction of
-- the taint to the set
addTaintedSink :: (Ord v) => OverflowDomain v -> S.Set InstructionIndex -> v -> S.Set InstructionIndex
addTaintedSink od tainted v
  | Just ix <- M.lookup v (odTainted od) = S.insert ix tainted
  | otherwise = tainted

emptyOverflowDomain :: OverflowDomain v
emptyOverflowDomain = OverflowDomain { odOverflowSources = M.empty
                                     , odTainted = M.empty
                                     , odUsedWhileTainted = S.empty
                                     }

      -- TODO(conathan): constructing instructions directly here won't
      -- work since the ISA and ABI are abstract. Right now this code
      -- only works for x86. Generalize it later when it stabilizes.
      -- let hlt = x64MakeInstr "hlt" []
      -- let int3 = x64MakeInstr "int3" [] -- Good for interactive tests in GDB.
      -- let trap = int3 -- try this with 'hlt' as well and see what happens
      -- let lengthOfTrap = isaInstructionSize isa trap
      -- let jumpOffset = D.JumpOffset D.ZSize (fromIntegral lengthOfTrap)
      -- -- The 'jno' jumps if "no overflow"; here "overflow" means
      -- -- "signed overflow'.
      -- let jno = x64MakeInstr "jno" [jumpOffset]
      -- -- The 'jnc' jumps if "no carry"; here "carry" means "unsigned
      -- -- overflow'.
      -- let jnc = x64MakeInstr "jnc" [jumpOffset]
      -- Trap sequence. In
      -- ':/tools/measure/examples/manual-instrumentation' we use the
      -- instrumentation:
      --
      --     jc .L_trap
      --     jo .L_trap
      --     jmp .L_safe
      --     .L_trap
      --     int3
      --     .L_safe
      --
      -- but here we use
      --
      --     jnc .L_no_carry
      --     int3
      --     .L_no_carry:
      --     jno .L_no_overflow:
      --     int3
      --     .L_no_overflow:
      --
      -- since the relative jumps are easier to compute.
      -- return [ i
      --        , (jno, Nothing)
      --        , (trap, Nothing)
      --        , (jnc, Nothing)
      --        , (trap, Nothing)
      --        ]

-- ??? What is the 'Maybe SymbolicAddress' for? A: it's a symbolic
-- version of the target of the current instruction, in case the
-- current instruction is a jump. Tristan says only the last
-- instruction in the sequence should have one, so I think it would
-- make sense to change the 'SymbolicBlock' type to enforce
-- this. Would simplify things I think.

-- ??? What is 'recordInstrumentation' for?

