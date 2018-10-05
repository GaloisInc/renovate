{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Lift concrete blocks into relocatable symbolic blocks
module Renovate.Redirect.Symbolize (
  symbolizeBasicBlocks
  ) where

import           Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Traversable as T

import           Prelude

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Redirect.Monad

-- import Debug.Trace

-- | Convert concrete blocks into symbolic blocks.
--
-- This requires:
--
-- 1) assigning symbolic addresses to each basic block, and
--
-- 2) updating all of the (direct) jump instructions with their
--    symbolic targets.
--
-- Indirect jumps do not need to be annotated (in part because we
-- cannot annotate them).
symbolizeBasicBlocks :: (Monad m, T.Traversable t, InstructionConstraints arch)
                     => t (ConcreteBlock arch)
                     -> RewriterT arch m (t (ConcreteBlock arch, SymbolicBlock arch))
symbolizeBasicBlocks concreteBlocks = do
  symBlocks <- T.traverse allocateSymbolicBlockAddress concreteBlocks
  let blockAddressIndex = M.fromList [ (basicBlockAddress b, symAddr)
                                     | (b, symAddr) <- F.toList symBlocks
                                     ]
  T.traverse (symbolizeJumps blockAddressIndex) symBlocks
  where
    allocateSymbolicBlockAddress cb = (cb,) <$> nextSymbolicAddress

-- | For the given 'SymbolicBlock', traverse all of its instructions
-- and tag them with their symbolic destination, if any.
--
-- We can find the symbolic destination based from the map of absolute
-- addresses to symbolic blocks.
--
-- For relative jumps, we need to compute the address of the
-- instruction and then add the offset of the relative jump (and look
-- the result up in that same map).
--
-- See Note [Jump Promotion]
symbolizeJumps :: forall arch m
                . (Monad m, InstructionConstraints arch)
               => M.Map (ConcreteAddress arch) (SymbolicAddress arch)
               -> (ConcreteBlock arch, SymbolicAddress arch)
               -> RewriterT arch m (ConcreteBlock arch, SymbolicBlock arch)
symbolizeJumps symAddrMap (cb, symAddr) = do
  isa <- askISA
  mem <- askMem
  let lookupSymAddr ca = M.lookup ca symAddrMap
  insns <- T.forM (instructionAddresses isa cb) $ \(i, addr) -> do
    case isaJumpType isa i mem addr of
      AbsoluteJump _ target -> do
        symTarget <- lookupSymbolicAddress target
        return $ isaSymbolizeAddresses isa mem lookupSymAddr addr (Just symTarget) i
      RelativeJump _ _ offset
        | absoluteAddress addr + fromIntegral offset < absoluteAddress concAddr + fromIntegral (concreteBlockSize isa cb) ->
          return $ isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
        | otherwise -> do
          symTarget <- lookupSymbolicAddress (addr `addressAddOffset` offset)
          return $ isaSymbolizeAddresses isa mem lookupSymAddr addr (Just symTarget) i
      IndirectJump _ ->
        -- We do not know the destination of indirect jumps, so we
        -- can't tag them (or rewrite them later)
        return $ isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
      DirectCall _ offset -> do
        symTarget <- lookupSymbolicAddress (addr `addressAddOffset` offset)
        return $ isaSymbolizeAddresses isa mem lookupSymAddr addr (Just symTarget) i
      IndirectCall ->
        return $ isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
      Return ->
        return $ isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
      NoJump -> return $ isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
  return (cb, BasicBlock { basicBlockAddress = SymbolicInfo { symbolicAddress = symAddr
                                                            , concreteAddress = concAddr
                                                            }
                         , basicBlockInstructions = concat insns
                         })
  where
    concAddr = basicBlockAddress cb
    lookupSymbolicAddress target =
      case M.lookup target symAddrMap of
        Just saddr -> return saddr
        Nothing -> return (StableAddress target)

{- Note [Jump Promotion]

While we are making addresses symbolic, we also have to promote short jump
instructions to full length jumps in case we need to jump farther during code
relocation.  We use 'isaModifyJumpTarget' for that purpose, as it chooses the
biggest jump that it can.  Unfortunately, due to the types, we have to do that
*before* we call 'isaSymbolizeAddresses', which changes the operand annotation.
The actual address used for isaModifyJumpTarget isn't important, as we'll tag on
the correct symbolic address immediately.

This happens to be fine for now, as 'isaSymbolizeAddresses' only deals with
operands that deal with memory addresses, while 'isaModifyJumpTarget' only deals
with operands of jump instructions.

This probably only affects x86.

-}
