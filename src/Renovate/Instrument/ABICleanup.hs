{-# LANGUAGE TupleSections #-}
-- | Clean up all caller-save registers before returning from a
-- function.
module SFE.Instrument.ABICleanup (
  abiCleanup
  ) where

import SFE.ABI
import SFE.BasicBlock
import SFE.Instrument.Monad

instrumentationTypeName :: String
instrumentationTypeName = "ABICleanup"

-- | An instrumentor function that cleans up caller-save registers before returns.
--
-- The function is parametric in the ABI it operates over.  Different
-- ABIs on the same platform could (in theory) have different sets of
-- caller-save registers.
abiCleanup :: ABI i a r w -> SymbolicBlock i a w -> Instrument i w [TaggedInstruction i a]
abiCleanup abi b = do
  case reverse insns of
    [] -> return []
    lastInsn:rest
      | isReturn abi (projectInstruction lastInsn) -> do
          let clearInsns = map (tagInstruction Nothing . clearRegister abi) (callerSaveRegisters abi)
              retIdx = fromIntegral (length insns - 1)
          recordInstrumentation instrumentationTypeName blockAddr retIdx
          return (reverse (lastInsn : (clearInsns ++ rest)))
      | otherwise -> return insns
  where
    blockAddr = basicBlockAddress b
    insns = basicBlockInstructions b
