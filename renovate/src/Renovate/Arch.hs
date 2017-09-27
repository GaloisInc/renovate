-- | This module defines all of the supported architectures for Renovate
--
-- These architecture tags are used to select backends at run time.  Callers can
-- provide the backends they want to support by depending on
-- architecture-specific backends (e.g., renovate-x86).
module Renovate.Arch (
  Architecture(..)
  ) where

data Architecture = X86_64
                  | PPC64
                  | PPC32
                  | ARM
                  | AArch64
                  deriving (Eq, Ord, Show)
