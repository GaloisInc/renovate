{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Renovate.Core.Chunk (
  Chunk(..)
  , chunkAddress
  , chunkSize
  ) where

import qualified Data.ByteString as B
import qualified Data.Macaw.CFG as MC
import           Data.Word ( Word64 )
import qualified Prettyprinter as PP

import qualified Renovate.Core.Address as RCA
import qualified Renovate.Core.BasicBlock as RCB
import qualified Renovate.ISA as RI

-- | A chunk is a unit of code managed during re-assembly
--
-- A chunk of code can either be a 'RCB.ConcretizedBlock' (i.e., a block that
-- has had a new address assigned to it) or raw bytes that have been assigned an
-- address (either raw injected code or padding bytes).
data Chunk arch = BlockChunk (RCB.ConcretizedBlock arch)
                | RawChunk (RCA.ConcreteAddress arch) B.ByteString

deriving instance (MC.MemWidth (MC.ArchAddrWidth arch)) => Show (Chunk arch)

instance (MC.MemWidth (MC.ArchAddrWidth arch)) => PP.Pretty (Chunk arch) where
  pretty (BlockChunk b) = PP.pretty b
  pretty (RawChunk addr bs) = PP.pretty addr PP.<> PP.pretty "@" PP.<> PP.pretty (B.length bs)

-- | Get the address assigned to this 'Chunk'
chunkAddress :: Chunk arch -> RCA.ConcreteAddress arch
chunkAddress c =
  case c of
    BlockChunk b -> RCB.concretizedBlockAddress b
    RawChunk addr _ -> addr

-- | Compute the size of this 'Chunk'
chunkSize :: RI.ISA arch -> Chunk arch -> Word64
chunkSize isa c =
  case c of
    BlockChunk b -> RCB.blockSize isa b
    RawChunk _ b -> fromIntegral (B.length b)
