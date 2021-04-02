{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeInType #-}
-- | Defines a function to convert from symbolic blocks to concrete
-- blocks.
module Renovate.Redirect.Concretize (
    InjectConcreteInstructions(..)
  , concretize
  ) where

import           Control.Exception ( assert )
import           Control.Monad.IO.Class ( MonadIO )
import qualified Control.Monad.State as S
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Map as M
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Traversable as T
import           Data.Typeable ( Typeable )
import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe, maybeToList )
import           Data.Monoid ( Sum(Sum) )
import           Data.Word ( Word64 )

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA
import qualified Renovate.Panic as RP
import           Renovate.Recovery ( BlockInfo(biCFG) )
import           Renovate.Redirect.LayoutBlocks ( Layout(..), layoutBlocks )
import           Renovate.Redirect.LayoutBlocks.Types ( WithProvenance(..)
                                                      , changed
                                                      , LayoutStrategy
                                                      )
import           Renovate.Redirect.Monad
import qualified Renovate.Rewrite as RRW

data InjectConcreteInstructions arch where
  InjectConcreteInstructions :: (ArchConstraints arch tp)
                             => InstructionArchRepr arch tp
                             -> DLN.NonEmpty (Instruction arch tp ())
                             -> InjectConcreteInstructions arch

concreteAddressMap
  :: Layout AddressAssignedBlock i arch
  -> M.Map (SymbolicAddress arch) (ConcreteAddress arch)
concreteAddressMap layout =
  mconcat [ concreteBlockAddressMap
          , injectedBlobAddressMap
          , injectedInstructionAddressMap
          ]
  where
    concreteBlockAddresses = programBlockLayout layout
    concreteBlockAddressMap =
      M.fromList [ (symbolicBlockSymbolicAddress sb, ca)
                 | wp <- F.toList concreteBlockAddresses
                 , let aab = withoutProvenance wp
                 , let sb = lbBlock aab
                 , let ca = lbAt aab
                 ]
    injectedBlobAddressMap =
        M.fromList [ (symAddr, concAddr)
                   | (symAddr, concAddr, _) <- injectedBlockLayout layout
                   ]
    injectedInstructionAddressMap =
        M.fromList [ (symAddr, concAddr)
                   | (symAddr, concAddr, _) <- injectedInstructionLayout layout
                   ]


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
concretize :: (MonadIO m, T.Traversable t, InstructionConstraints arch, Typeable arch)
           => LayoutStrategy
           -> ConcreteAddress arch
           -- ^ The start address of the concretized (instrumented) blocks
           -> t (WithProvenance SymbolicBlock arch)
           -> t (SymbolicAddress arch, BS.ByteString)
           -> t (SymbolicAddress arch, RRW.InjectSymbolicInstructions arch)
           -> BlockInfo arch
           -> RewriterT arch m ( Layout ConcretizedBlock InjectConcreteInstructions arch
                               , M.Map (SymbolicAddress arch) (ConcreteAddress arch)
                               )
concretize strat startAddr blocks injectedCode injectedInstructions blockInfo = do
  -- First, build up a mapping of symbolic address to new concrete
  -- address
  symmap <- askSymbolMap
  layout <- layoutBlocks strat startAddr blocks injectedCode injectedInstructions (biCFG blockInfo)
  let symToConcAddrs = concreteAddressMap layout

      -- Make note of symbolic names for each embrittled function. We can
      -- use this to make new symtab entries for them.
  let brittleMap = M.fromList [ (ca, (concreteBlockAddress oa, nm))
                              | wp <- F.toList (programBlockLayout layout)
                              , let oa = originalBlock wp
                              , let aab = withoutProvenance wp
                              , let ca = lbAt aab
                              , nm <- maybeToList $ Map.lookup (concreteBlockAddress oa) symmap
                              ]
  -- TODO: JED: Should this be a put or an append?
  putNewSymbolsMap brittleMap
  -- Now go through and fix up all of the jumps to symbolic addresses (which
  -- happen to occur at the end of basic blocks).  Note that we only traverse
  -- the concrete blocks here, not the injected blocks.
  v <- T.traverse (concretizeJumps symToConcAddrs) (programBlockLayout layout)
  concretizedInjectedInstructions <- mapM (concretizeInjectedInstructions symToConcAddrs) (injectedInstructionLayout layout)
  let concLayout = Layout { programBlockLayout = v
                          , layoutPaddingBlocks = layoutPaddingBlocks layout
                          , injectedBlockLayout = injectedBlockLayout layout
                          , injectedInstructionLayout = concretizedInjectedInstructions
                          }
  return (concLayout, symToConcAddrs)

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

neconcat :: DLN.NonEmpty (DLN.NonEmpty a) -> DLN.NonEmpty a
neconcat nel =
  case nel of
    (e0 DLN.:| rest) DLN.:| others ->
      e0 DLN.:| (rest <> concat (fmap F.toList others))

concretizeInjectedInstructions
  :: (Monad m, InstructionConstraints arch)
  => M.Map (SymbolicAddress arch) (ConcreteAddress arch)
  -> (SymbolicAddress arch, ConcreteAddress arch, RRW.InjectSymbolicInstructions arch)
  -> RewriterT arch m (SymbolicAddress arch, ConcreteAddress arch, InjectConcreteInstructions arch)
concretizeInjectedInstructions symToConcAddrs (symAddr, concAddr, RRW.InjectSymbolicInstructions repr insns) = do
  let conc = T.traverse (concretizeAddresses symToConcAddrs) insns
  concInstrsWithSizes <- S.evalStateT conc concAddr
  let concInstrs = neconcat (fmap fst concInstrsWithSizes)
  return (symAddr, concAddr, InjectConcreteInstructions repr concInstrs)

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
--
-- NOTE: The implementation of concretization MUST match the logic used to
-- compute block sizes in 'symbolicBlockSize' in Renovate.BasicBlock.
concretizeJumps :: (Monad m, InstructionConstraints arch)
                => M.Map (SymbolicAddress arch) (ConcreteAddress arch)
                -> WithProvenance AddressAssignedBlock arch
                -> RewriterT arch m (WithProvenance ConcretizedBlock arch)
concretizeJumps symToConcAddrs wp
  | changed status = withSymbolicInstructions sb $ \repr symInsns -> do
    -- Concretize all of the instructions (including jumps)
    let concretizeInstrs = T.traverse (concretizeAddresses symToConcAddrs) symInsns
    (concretizedInstrsWithSizes, nextAddr) <- S.runStateT concretizeInstrs firstInstrAddr
    let Sum baseBlockSize = foldMap snd concretizedInstrsWithSizes
    let baseBlockInstrs = foldMap (F.toList . fst) concretizedInstrsWithSizes

    -- Now figure out if we have a fallthrough target; if we do, we'll put an
    -- unconditional jump to the fallthrough target at the end of our new
    -- instruction stream.
    --
    -- This handling of fallthrough is totally disjoint from the rest of the
    -- instruction stream, which means that we can support rewriting workflows
    -- like inserting instrumentation after a function call (which is a block
    -- terminator), while still correcting fallthrough control flow when needed.
    let mConcreteFallthroughTarget =
          case symbolicBlockSymbolicSuccessor sb of
            Nothing -> Nothing
            -- Stable addresses are not in the map
            Just (StableAddress concSucc) -> Just concSucc
            Just symSucc
              | Just concSucc <- M.lookup symSucc symToConcAddrs -> Just concSucc
              | otherwise ->
                RP.panic RP.Concretize "concretizeJumps" [ "Could not find symbolic successor for original block: " ++ show firstInstrAddr
                                                         ]

    isa <- askISA
    let (instrs, concretizedBlockSize) = fromMaybe (baseBlockInstrs, baseBlockSize) $ do
          concreteFallthroughTarget <- mConcreteFallthroughTarget
          -- The fallthrough will be at 'nextAddr' (following the last instruction)
          let jmp = isaMakeRelativeJumpTo isa nextAddr concreteFallthroughTarget repr
          let jmpSize = sum (fmap (isaInstructionSize isa) jmp)
          return (baseBlockInstrs ++ F.toList jmp, baseBlockSize + fromIntegral jmpSize)

    -- We computed a maximum possible size earlier; ensure that we stay within
    -- that bound (if we exceed it, all of the address calculations are off and
    -- the layout is invalid and broken)
    assert (concretizedBlockSize <= maxSize) (return ())

    let padding = isaMakePadding isa (maxSize - concretizedBlockSize) repr
    case DLN.nonEmpty (instrs ++ padding) of
      Nothing ->
        RP.panic RP.Concretize "concretizeJumps" [ "Generated an empty basic block at address: " ++ show firstInstrAddr
                                                 , "  for original block: " ++ show (concreteBlockAddress cb)
                                                 ]
      Just instrs' -> do
        let sb' = concretizedBlock firstInstrAddr instrs' repr
        return $! WithProvenance cb sb' status
  | otherwise = do
      withConcreteInstructions cb $ \repr insns -> do
        let cb' = concretizedBlock (concreteBlockAddress cb) insns repr
        return $ WithProvenance cb cb' status
  where
    cb = originalBlock wp
    aab = withoutProvenance wp
    sb = lbBlock aab
    firstInstrAddr = lbAt aab
    maxSize = lbSize aab
    status = rewriteStatus wp

concretizeAddresses :: forall m arch tp
                     . (Monad m, InstructionConstraints arch)
                    => M.Map (SymbolicAddress arch) (ConcreteAddress arch)
                    -> TaggedInstruction arch tp (InstructionAnnotation arch)
                    -> S.StateT (ConcreteAddress arch) (RewriterT arch m) (DLN.NonEmpty (Instruction arch tp ()), Sum Word64)
concretizeAddresses symToConcAddrs taggedInstr = do
  insnAddr <- S.get
  isa <- S.lift askISA
  mem <- S.lift askMem
  let insns = withConcTarget (isaConcretizeAddresses isa mem insnAddr (projectInstruction taggedInstr))
  let size = sum (fmap (toInteger . isaInstructionSize isa) insns)
  S.put (insnAddr `addressAddOffset` fromIntegral size)
  return (insns, fromIntegral size)
  where
    withConcTarget :: forall a . (forall tk . RelocatableTarget arch ConcreteAddress tk -> a) -> a
    withConcTarget k =
      case symbolicTarget taggedInstr of
        Some NoTarget -> k NoTarget
        Some (RelocatableTarget symTarget) ->
          let concreteTarget = lookupConcreteTarget symToConcAddrs symTarget
          in k (RelocatableTarget concreteTarget)

-- | Map from a symbolic address to a concrete one, failing if there is no entry
-- in the mapping
lookupConcreteTarget :: (InstructionConstraints arch)
                     => M.Map (SymbolicAddress arch) (ConcreteAddress arch)
                     -> SymbolicAddress arch
                     -> ConcreteAddress arch
lookupConcreteTarget symAddrMap symAddr =
  case symAddr of
    StableAddress concAddr -> concAddr
    SymbolicAddress {}
      | Just concAddr <- M.lookup symAddr symAddrMap -> concAddr
      | otherwise ->
        RP.panic RP.Concretize "lookupConcreteTarget" [ "No concrete target found for symbolic address: " ++ show symAddr
                                                      ]
