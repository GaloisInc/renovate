{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Lift concrete blocks into relocatable symbolic blocks
module Renovate.Redirect.Symbolize (
  symbolizeBasicBlocks
  ) where

import           GHC.TypeLits ( KnownNat )

import           Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Traversable as T
import           Data.Typeable ( Typeable )

import           Prelude

import qualified Data.Macaw.Memory as MM

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Redirect.Monad

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
symbolizeBasicBlocks :: (Monad m, T.Traversable t, KnownNat w, MM.MemWidth w, Typeable w)
                     => MM.Memory w
                     -> t (ConcreteBlock i w)
                     -> RewriterT i a w m (t (ConcreteBlock i w, SymbolicBlock i a w))
symbolizeBasicBlocks mem concreteBlocks = do
  symBlocks <- T.traverse allocateSymbolicBlockAddress concreteBlocks
  let blockAddressIndex = M.fromList [ (basicBlockAddress b, symAddr)
                                     | (b, symAddr) <- F.toList symBlocks
                                     ]
  T.traverse (symbolizeJumps mem blockAddressIndex) symBlocks
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
symbolizeJumps :: forall i a w m
                . (Monad m, KnownNat w, MM.MemWidth w, Typeable w)
               => MM.Memory w
               -> M.Map (RelAddress w) SymbolicAddress
               -> (ConcreteBlock i w, SymbolicAddress)
               -> RewriterT i a w m (ConcreteBlock i w, SymbolicBlock i a w)
symbolizeJumps mem symAddrMap (cb, symAddr) = do
  isa <- askISA
  insns <- T.forM (instructionAddresses isa cb) $ \(i, addr) -> do
    case isaJumpType isa i mem addr of
      AbsoluteJump _ target -> do
        symTarget <- lookupSymbolicAddress target
        return $ tag (isaSymbolizeAddresses isa addr (promoteJump isa i)) (Just symTarget)
      RelativeJump _ _ offset -> do
        symTarget <- lookupSymbolicAddress (addr `addressAddOffset` offset)
        return $ tag (isaSymbolizeAddresses isa addr (promoteJump isa i)) (Just symTarget)
      IndirectJump _ ->
        -- We do not know the destination of indirect jumps, so we
        -- can't tag them (or rewrite them later)
        return $ tag (isaSymbolizeAddresses isa addr (promoteJump isa i)) Nothing
      DirectCall _ offset -> do
        symTarget <- lookupSymbolicAddress (addr `addressAddOffset` offset)
        return $ tag (isaSymbolizeAddresses isa addr i) (Just symTarget)
      IndirectCall ->
        return $ tag (isaSymbolizeAddresses isa addr i) Nothing
      Return ->
        return $ tag (isaSymbolizeAddresses isa addr i) Nothing
      NoJump -> return $ tag (isaSymbolizeAddresses isa addr i) Nothing
  return (cb, BasicBlock { basicBlockAddress = SymbolicInfo { symbolicAddress = symAddr
                                                            , concreteAddress = concAddr
                                                            }
                         , basicBlockInstructions = insns
                         })
  where
    promoteJump isa i =
      let Just [pj] = isaModifyJumpTarget isa i concAddr concAddr
      in pj
    concAddr = basicBlockAddress cb
    lookupSymbolicAddress target =
      case M.lookup target symAddrMap of
        Just saddr -> return saddr
        Nothing -> do
          let err :: Diagnostic
              err = NoSymbolicAddressForTarget target "symbolizeJumps"
          logDiagnostic err
          throwError err

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
