{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | Defines a function to convert from symbolic blocks to concrete
-- blocks.
module Renovate.Redirect.Concretize ( concretize ) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Data.Map as Map
import           Data.Maybe ( maybeToList )

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Redirect.LayoutBlocks ( layoutBlocks )
import           Renovate.Redirect.LayoutBlocks.Types ( LayoutPair(..)
                                                      , SymbolicPair(..)
                                                      , AddressAssignedPair(..)
                                                      , ConcretePair(..)
                                                      , Status(..)
                                                      , LayoutStrategy )
import           Renovate.Redirect.Monad

-- | Take the rewritten symbolic blocks and assign them concrete
-- addresses.  This includes rewriting jump instructions to refer to
-- their new absolute targets.
--
-- While we could come up with minimal relative jump encodings, we use
-- constant-size relative jumps for simplicity.  While this could be very
-- complicated due to some instructions getting a bit longer (i.e.,
-- they were short jumps but become long jumps after rewriting), we'll
-- just leave enough padding after each function to ensure it isn't an
-- issue.  More specifically, the problem is that we don't know what
-- addresses to assign each block until we know how long each jump
-- will be.  I guess it doesn't need to be a problem if we just decide
-- to always use absolute jumps.
--
-- Note that blocks have to be laid out in order; using M.toList is
-- sufficient to sort by original address, which maintains the order
-- invariant.
concretize :: (Monad m, T.Traversable t, InstructionConstraints arch)
           => LayoutStrategy
           -> ConcreteAddress arch
           -- ^ The start address of the concretized (instrumented) blocks
           -> t (SymbolicPair arch)
           -> RewriterT arch m [ConcretePair arch]
concretize strat startAddr blocks = do
  -- First, build up a mapping of symbolic address to new concrete
  -- address
  isa <- askISA
  symmap <- askSymbolMap
  concreteAddresses <- layoutBlocks strat startAddr blocks
  let concreteAddressMap = M.fromList [ (symbolicAddress (basicBlockAddress sb), ca)
                                      | AddressAssignedPair (LayoutPair _ (AddressAssignedBlock sb ca) _) <- F.toList concreteAddresses
                                      ]
      -- Make note of symbolic names for each embrittled function. We can
      -- use this to make new symtab entries for them.
      brittleMap = M.fromList [ (ca, (basicBlockAddress oa, nm))
                              | AddressAssignedPair (LayoutPair oa (AddressAssignedBlock _sb ca) _) <- F.toList concreteAddresses
                              , nm <- maybeToList $ Map.lookup (basicBlockAddress oa) symmap
                              ]
  -- TODO: JED: Should this be a put or an append?
  putNewSymbolsMap brittleMap
  -- Now go through and fix up all of the jumps to symbolic addresses
  -- (which happen to occur at the end of basic blocks).
  T.traverse (concretizeJumps isa concreteAddressMap) concreteAddresses

{-

After we have the original concrete block and the new one with its
allocated address, we need to compute the address of each instruction
in each block and adjust the IP-relative memory references.

This will be an extra function of type

> (i -> i) -> ConcreteBlock i -> ConcreteBlock i


The metadata would be hard to deal with.

Instead, when creating the symbolic block, we could have a
pre-processing pass for the instructions in the symbolic block that
converts IP-relative memory references to absolute references.  We
could introduce forms with two Nothing values (no base and no index);
these could be sentinels that require translation back to IP relative

-}

-- | Convert jumps to symbolic addresses in 'SymbolicBlock's to concrete jumps.
-- This essentially converts a 'SymbolicBlock' to a 'ConcreteBlock'.
--
-- Note that this replaces each symbolic jump instruction with another
-- instruction (or sequence of instructions).  This could be a problem
-- if the original jump would have caused some side effect to restore
-- the stack or register state to what the compiler was expecting.  I
-- don't know of relevant examples off hand, but in the case of
-- something like @push XXX ; ret@, the @ret@ restores the stack
-- height and gets rid of the extra value.  We wouldn't change that in
-- this transformation, but the idea might be worth considering.
concretizeJumps :: (Monad m, InstructionConstraints arch)
                => ISA arch
                -> M.Map (SymbolicAddress arch) (ConcreteAddress arch)
                -> AddressAssignedPair arch
                -> RewriterT arch m (ConcretePair arch)
concretizeJumps isa concreteAddressMap (AddressAssignedPair (LayoutPair cb (AddressAssignedBlock sb baddr) Modified)) = do
  mem <- askMem
  let insnAddrs = instructionAddresses' isa (isaConcretizeAddresses isa mem baddr . projectInstruction) baddr (basicBlockInstructions sb)
  concretizedInstrs <- T.traverse (mapJumpAddress concreteAddressMap) insnAddrs
  let sb' = sb { basicBlockAddress = baddr
               , basicBlockInstructions = concretizedInstrs
               }
  return (ConcretePair (LayoutPair cb sb' Modified))
concretizeJumps _isa _concreteAddressMap (AddressAssignedPair (LayoutPair cb _ Unmodified)) =
  return (ConcretePair (LayoutPair cb cb Unmodified))

-- | We need the address of the instruction, so we need to pre-compute
-- all instruction addresses above.
--
-- To fix up jumps at the end of blocks, we need to keep the same jump
-- type, but change its target.  This may require changing the opcode,
-- as a longer jump may require a different instruction (e.g., 8 bit
-- to 32 bit offset).
mapJumpAddress :: forall m arch
                . (Monad m, InstructionConstraints arch)
               => M.Map (SymbolicAddress arch) (ConcreteAddress arch)
               -> (TaggedInstruction arch (InstructionAnnotation arch), ConcreteAddress arch)
               -> RewriterT arch m (Instruction arch ())
mapJumpAddress concreteAddressMap (tagged, insnAddr) = do
  isa <- askISA
  mem <- askMem
  case symbolicTarget tagged of
    Just (StableAddress concAddr) ->
      case isaModifyJumpTarget isa (isaConcretizeAddresses isa mem insnAddr i) insnAddr concAddr of
        Nothing -> do
          let err :: Diagnostic
              err = InstructionIsNotJump (show i)
          logDiagnostic err
          throwError err
        Just insn -> return insn
    Just symAddr@(SymbolicAddress _)
      | Just concAddr <- M.lookup symAddr concreteAddressMap ->
        case isaModifyJumpTarget isa (isaConcretizeAddresses isa mem insnAddr i) insnAddr concAddr of
          Nothing -> do
            let err :: Diagnostic
                err = InstructionIsNotJump (show i)
            logDiagnostic err
            throwError err
          Just insn -> return insn
      | otherwise -> do
          let err :: Diagnostic
              err = NoConcreteAddressForSymbolicTarget symAddr "concretizeJumps"
          logDiagnostic err
          throwError err
    Nothing -> return (isaConcretizeAddresses isa mem insnAddr i)
  where
    i = projectInstruction tagged

