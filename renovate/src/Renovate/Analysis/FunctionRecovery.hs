{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
-- | A simple function recovery pass
--
-- This module performs a simple forward reachability pass from the
-- function roots that are discovered by macaw.
--
-- A more sophisticated version will be written eventually, probably
-- built in to macaw.  Note that this version does not handle indirect
-- jumps (including jump tables).  Only blocks reachable from direct
-- jumps will be in the CFG.
module Renovate.Analysis.FunctionRecovery (
  recoverFunctions,
  Completion(..),
  FunctionCFG(..)
  ) where

import qualified GHC.Err.Located as L

import Control.Applicative
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Data.Macaw.Memory as MM

import Prelude

import Renovate.Address
import Renovate.BasicBlock
import Renovate.ISA
import Renovate.Recovery

-- | Mark a CFG as complete or incomplete
--
-- A complete CFG has all of its basic blocks and all of the
-- connections between them.
--
-- An incomplete CFG is missing some blocks due to e.g., indirect
-- jumps that could not be resolved.
data Completion = Complete
                | Incomplete
                deriving (Eq, Ord, Show)

data FunctionCFG i w = FunctionCFG { cfgEntry :: RelAddress w
                                   -- ^ The entry point of the function
                                   , cfgSuccessors :: M.Map (RelAddress w) [RelAddress w]
                                   -- ^ The successors of each block
                                   , cfgExitBlocks :: [RelAddress w]
                                   -- ^ Exit blocks of the function
                                   , cfgBlocks :: M.Map (RelAddress w) (ConcreteBlock i w)
                                   -- ^ All of the blocks in the CFG
                                   , cfgCompletion :: Completion
                                   -- ^ Whether or not the CFG is complete
                                   }

-- | Starting from a basic set of recovered block information from
-- macaw, assign basic blocks to functions and construct CFGs.
recoverFunctions :: (MM.MemWidth w)
                 => ISA i a w
                 -> MM.Memory w
                 -> BlockInfo i w arch
                 -> [FunctionCFG i w]
recoverFunctions isa mem blockInfo = runM isa mem blockInfo $ do
  F.foldlM buildCFG [] (biFunctionEntries blockInfo)

-- | Given a function entry point, traverse forward across all of its
-- basic blocks that we can find to build up a CFG.
--
-- If we encounter an indirect jump (that isn't a call), mark the CFG
-- as incomplete.
--
-- We do not follow call edges.
buildCFG :: (MM.MemWidth w) => [FunctionCFG i w] -> RelAddress w -> M i a w [FunctionCFG i w]
buildCFG acc entryAddr = do
  mcfg <- makeCFGForEntry entryAddr
  case mcfg of
    Just cfg -> return (cfg : acc)
    Nothing -> return acc

{-

Take a worklist approach with a local state monad where we build up
the components of a function CFG.  Examine the last instruction of the
block:

 * If it is a jump, figure out the jump type and add successors to the
   worklist, if we can identify them (if we can't find them, toggle
   hasIndirectJump)

 * If it is a call, add the address of the instruction after the call
   to the worklist

 * If it is not a terminator at all, add the next instruction to the
   worklist

-}

makeCFGForEntry :: (MM.MemWidth w) => RelAddress w -> M i a w (Maybe (FunctionCFG i w))
makeCFGForEntry entryAddr = do
  addBlockToWorklist entryAddr
  processWorklist
  st <- resetState
  return $ Just $ FunctionCFG { cfgEntry = entryAddr
                              , cfgSuccessors = fmap F.toList (fsSuccessors st)
                              , cfgExitBlocks = F.toList (fsExitBlocks st)
                              , cfgBlocks = fsBlocks st
                              , cfgCompletion = if fsHasIndirectJump st then Incomplete else Complete
                              }

processWorklist :: (MM.MemWidth w) => M i a w ()
processWorklist = do
  wl <- RWS.gets fsWorklist
  case S.minView wl of
    Nothing -> return ()
    Just (addr, rest) -> do
      RWS.modify' $ \s -> s { fsVisited = S.insert addr (fsVisited s)
                            , fsWorklist = rest
                            }
      Just b <- M.lookup addr <$> RWS.asks envBlocks
      isa <- RWS.asks envISA
      mem <- RWS.asks envMem
      let addOff = addressAddOffset mem
      case instructionAddresses isa mem b of
        [] -> L.error "Empty basic block"
        insns -> do
          let (lastInsn, insnAddr) = last insns
          case isaJumpType isa lastInsn mem insnAddr of
            -- Fallthrough to the next block
            NoJump -> do
              successor <- nextBlockAddress b
              addCFGEdge addr successor
            Return -> addReturnBlock addr
            RelativeJump Unconditional jaddr off -> do
              let target = jaddr `addOff` off
              addCFGEdge addr target
            RelativeJump Conditional jaddr off -> do
              let target = jaddr `addOff` off
              successor <- nextBlockAddress b
              addCFGEdge addr target
              addCFGEdge addr successor
            AbsoluteJump Unconditional dst -> do
              addCFGEdge addr dst
            AbsoluteJump Conditional dst -> do
              successor <- nextBlockAddress b
              addCFGEdge addr dst
              addCFGEdge addr successor
            IndirectJump Conditional -> do
              successor <- nextBlockAddress b
              addCFGEdge addr successor
              markFunctionIncomplete
            DirectCall {} -> do
              successor <- nextBlockAddress b
              addCFGEdge addr successor
            IndirectCall -> do
              successor <- nextBlockAddress b
              addCFGEdge addr successor
            IndirectJump Unconditional -> markFunctionIncomplete
          processWorklist

nextBlockAddress :: (MM.MemWidth w) => ConcreteBlock i w -> M i a w (RelAddress w)
nextBlockAddress b = do
  isa <- RWS.asks envISA
  mem <- RWS.asks envMem
  let sz     = concreteBlockSize isa b
      addOff = addressAddOffset mem
  return (basicBlockAddress b `addOff` fromIntegral sz)

markFunctionIncomplete :: M i a w ()
markFunctionIncomplete = do
  RWS.modify' $ \s -> s { fsHasIndirectJump = True }

addReturnBlock :: RelAddress w -> M i a w ()
addReturnBlock addr =
  RWS.modify' $ \s -> s { fsExitBlocks = S.insert addr (fsExitBlocks s) }

{-# INLINE addCFGEdge #-}
addCFGEdge :: RelAddress w -> RelAddress w -> M i a w ()
addCFGEdge src dst = do
  RWS.modify' $ \s -> s { fsSuccessors = M.insertWith S.union src (S.singleton dst) (fsSuccessors s) }
  addBlockToWorklist dst

{-# INLINE addBlockToWorklist #-}
addBlockToWorklist :: RelAddress w -> M i a w ()
addBlockToWorklist addr = do
  RWS.modify' $ \s ->
    case S.member addr (fsVisited s) of
      True -> s
      False -> s { fsWorklist = S.insert addr (fsWorklist s) }

newtype M i a w t = M { unM :: RWS.RWS (CFGEnv i a w) () (FunctionState i w) t }
  deriving (Monad,
            Applicative,
            Functor,
            RWS.MonadReader (CFGEnv i a w),
            RWS.MonadState (FunctionState i w))

data CFGEnv i a w = CFGEnv { envBlocks :: M.Map (RelAddress w) (ConcreteBlock i w)
                           , envISA :: ISA i a w
                           , envMem :: MM.Memory w
                           }

data FunctionState i w = FunctionState { fsSuccessors :: M.Map (RelAddress w) (S.Set (RelAddress w))
                                       , fsExitBlocks :: S.Set (RelAddress w)
                                       , fsBlocks :: M.Map (RelAddress w) (ConcreteBlock i w)
                                       , fsHasIndirectJump :: Bool
                                       , fsVisited :: S.Set (RelAddress w)
                                       , fsWorklist :: S.Set (RelAddress w)
                                       }

emptyFunctionState :: FunctionState i w
emptyFunctionState = FunctionState { fsSuccessors = M.empty
                                   , fsExitBlocks = S.empty
                                   , fsBlocks = M.empty
                                   , fsHasIndirectJump = False
                                   , fsVisited = S.empty
                                   , fsWorklist = S.empty
                                   }

-- | Reset the function state, returning the old state
resetState :: M i a w (FunctionState i w)
resetState = do
  s <- RWS.get
  RWS.put emptyFunctionState
  return s

runM :: ISA i a w -> MM.Memory w -> BlockInfo i w arch -> M i a w t -> t
runM isa mem blockInfo act = fst $ RWS.evalRWS (unM act) env emptyFunctionState
  where
    env = CFGEnv { envBlocks = F.foldl' addBlock M.empty (biBlocks blockInfo)
                 , envISA = isa
                 , envMem = mem
                 }
    addBlock m b = M.insert (basicBlockAddress b) b m

