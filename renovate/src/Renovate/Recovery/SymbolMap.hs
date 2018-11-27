{-# LANGUAGE FlexibleContexts #-}
module Renovate.Recovery.SymbolMap (
  SymbolMap,
  NewSymbolsMap,
  toMacawSymbolMap
  ) where

import qualified Data.ByteString as B
import qualified Data.Map as M

import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MC

import           Renovate.Address ( ConcreteAddress, concreteAsSegmentOff )

type SymbolMap     arch = M.Map (ConcreteAddress arch) B.ByteString
type NewSymbolsMap arch = M.Map (ConcreteAddress arch) (ConcreteAddress arch, B.ByteString)

toMacawSymbolMap :: (MC.MemWidth (MC.ArchAddrWidth arch)) => MC.Memory (MC.ArchAddrWidth arch) -> SymbolMap arch -> IO (MC.AddrSymMap (MC.ArchAddrWidth arch))
toMacawSymbolMap mem sm = return (M.mapKeys toSegOff sm)
  where
    toSegOff concAddr =
      case concreteAsSegmentOff mem concAddr of
        Nothing -> error ("Invalid concrete address: " ++ show concAddr)
        Just so -> so
