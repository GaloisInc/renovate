{-# LANGUAGE FlexibleContexts #-}
-- | Instrument functions to add a shadow stack.
--
-- This transformation is more involved than the others - it involves:
--
-- 1) Some setup code that has to be inserted before _start
--
-- 2) A pre-transformation analysis to find function entries and exits
--
-- 3) Block rewriting to add shadow stack manipulation on function
--    entry and checks at function exit
--
-- Initially, we are going to use a very basic function recovery
-- method until we can integrate the more sophisticated version in
-- reopt (which should end up in macaw).
module SFE.Instrument.ShadowStack (
  addShadowStack
  ) where

import qualified GHC.Err.Located as L

import Data.Word ( Word32 )

import qualified Data.Macaw.Memory as MM

import SFE.ABI
import qualified SFE.Analysis.FunctionRecovery as FR
import SFE.BasicBlock
import SFE.ISA
import qualified SFE.Instrument as I

instrumentationTypeName :: String
instrumentationTypeName = "ShadowStack"

-- | The number of bytes to allocate for the shadow stack
--
-- FIXME: Later, we'll want to figure out the actual stack size from
-- the ELF info and allocate accordingly
shadowStackBytes :: Word32
shadowStackBytes = 2 * 1024 * 1024

-- | Add shadow stack manipulation and checks to the block.
--
-- If the block is both an entry and exit, we can skip it (since it
-- doesn't call any functions or jump at all).
--
-- If the block is also the entry to the program, we need to allocate
-- the shadow stack before doing anything else.
addShadowStack :: (MM.MemWidth w)
               => ABI i a r w
               -> ISA i a w
               -> SymbolicBlock i a w
               -> I.Instrument i w [TaggedInstruction i a]
addShadowStack abi isa sb = do
  mcfg <- I.lookupBlockCFG sb
  insns1 <- case mcfg of
    Just cfg
      | FR.cfgCompletion cfg == FR.Complete -> do
        let concAddr = concreteAddress (basicBlockAddress sb)
        let isCompleteEntry = FR.cfgEntry cfg == concAddr
        let isCompleteReturn = concAddr `elem` FR.cfgExitBlocks cfg
        case () of
          _ | isCompleteEntry && isCompleteReturn ->
              return (basicBlockInstructions sb)
            | isCompleteEntry ->
              instrumentEntryBlock abi isa sb
            | isCompleteReturn ->
              instrumentExitBlock abi isa sb
            | otherwise -> return (basicBlockInstructions sb)
    _ -> return (basicBlockInstructions sb)
  entryPoint <- I.lookupEntryAddress
  case concreteAddress (basicBlockAddress sb) == entryPoint of
    False -> return insns1
    True -> allocateShadowStack abi (basicBlockAddress sb) insns1

shadowStackOffsetVarName :: String
shadowStackOffsetVarName = "_sfe_shadow_stack_offset"

allocateShadowStack :: (MM.MemWidth w)
                    => ABI i a r w
                    -> SymbolicInfo w
                    -> [TaggedInstruction i a]
                    -> I.Instrument i w [TaggedInstruction i a]
allocateShadowStack abi blockAddr insns = do
  I.recordInstrumentation instrumentationTypeName blockAddr 0
  globalVarAddress <- I.newGlobalVar shadowStackOffsetVarName (fromIntegral (pointerSize abi))
  return $ concat [ fmap (tagInstruction Nothing) $ allocateMemory abi shadowStackBytes globalVarAddress
                  , fmap (tagInstruction Nothing) $ computeStackPointerOffset abi globalVarAddress
                  , insns
                  ]

-- | Put the return address onto the shadow stack
instrumentEntryBlock :: ABI i a r w
                     -> ISA i a w
                     -> SymbolicBlock i a w
                     -> I.Instrument i w [TaggedInstruction i a]
instrumentEntryBlock abi _isa sb = do
  I.recordInstrumentation instrumentationTypeName (basicBlockAddress sb) 0
  shadowStackOffset <- I.lookupGlobalVar shadowStackOffsetVarName
  let saveInsns = saveReturnAddress abi shadowStackOffset
  return (fmap (tagInstruction Nothing) saveInsns ++ basicBlockInstructions sb)

-- | Check the shadow stack return value against the real one
instrumentExitBlock :: ABI i a r w
                    -> ISA i a w
                    -> SymbolicBlock i a w
                    -> I.Instrument i w [TaggedInstruction i a]
instrumentExitBlock abi _isa b = do
  I.recordInstrumentation instrumentationTypeName (basicBlockAddress b) (fromIntegral (length insns - 1))
  shadowStackOffset <- I.lookupGlobalVar shadowStackOffsetVarName
  case reverse (basicBlockInstructions b) of
    [] -> L.error ("Empty basic block at " ++ show (basicBlockAddress b))
    (retInst:rest) -> return $ concat [ [retInst]
                                      , fmap (tagInstruction Nothing) $ checkShadowStack abi shadowStackOffset
                                      , rest
                                      ]
  where
    insns = basicBlockInstructions b

{- Note [Shadow Stack]



-}
