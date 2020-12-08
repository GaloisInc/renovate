{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-
Module : Renovate.BinaryFormat.ELF.Exceptions
Description : Exceptions that may arise during ELF rewriting
Copyright : (c) Galois, Inc 2020
License : BSD3
Maintainer : Tristan Ravitch <tristan@galois.com>
Stability : provisional
-}

{-# LANGUAGE LambdaCase #-}

module Renovate.BinaryFormat.ELF.Exceptions
  ( ElfRewritingException(..)
  , printELFRewritingException
  ) where

import qualified Control.Monad.Catch.Pure as P
import qualified Data.Binary.Get as DBG
import qualified Data.ElfEdit as EE
import           Data.Word (Word64)
import qualified GHC.Stack as Stack

import qualified Data.ElfEdit as E

-- | See 'wrapErrorMessage' for what the constructors mean.
data ExceptionClassification =
    MalformedInput
  | UserError
  | InternalError

wrapErrorMessage :: ExceptionClassification -> String -> String
wrapErrorMessage classifier msg =
  case classifier of
    MalformedInput -> "Malformed input: " ++ msg
    UserError -> "User error: " ++ msg
    InternalError ->
      concat [ "Internal error: "
             , msg
             , "\n\n"
             , "This is always a bug, please report at "
             , "https://github.com/GaloisInc/renovate/issues"
             ]

data ElfRewritingException =
    TooManyEXIDXs [E.SegmentIndex]
  | WrongEXIDXIndex E.SegmentIndex
  | NoSpaceForPHDRs Word64 Word64
  | CouldNotDecodeElf Stack.CallStack DBG.ByteOffset String
  | forall w . (Integral (EE.ElfWordType w)) => NoLoadableSegments [EE.Phdr w]

deriving instance Show ElfRewritingException

classifyException :: ElfRewritingException -> ExceptionClassification
classifyException =
  \case
    TooManyEXIDXs{} -> MalformedInput
    WrongEXIDXIndex{} -> MalformedInput
    NoSpaceForPHDRs{} -> InternalError
    CouldNotDecodeElf {} -> InternalError
    NoLoadableSegments {} -> InternalError

printELFRewritingException :: ElfRewritingException -> String
printELFRewritingException exception =
  wrapErrorMessage (classifyException exception) $
    case exception of
      WrongEXIDXIndex idx ->
        unwords
          [ "Expected EXIDX segment to have index 0, but it had index"
          , show idx
          ]
      TooManyEXIDXs idxs ->
        unwords
          [ "Found several ARM_EXIDX segments, at the following indices:"
          , show idxs
          ]
      NoSpaceForPHDRs offset size ->
        unlines
          [ "Internal error: Unable to find space for PHDR segment"
          , "Offset of PHDR segment: " ++ show offset
          , "Size of PHDR segment: " ++ show size
          ]
      CouldNotDecodeElf ctx off msg ->
        unwords
          [ "Could not decode encoded ELF in"
          , Stack.prettyCallStack ctx
          , "at offset"
          , show off
          , ":"
          , msg
          ]
      NoLoadableSegments phdrs -> ("No loadable segments: " ++ show phdrs)

instance P.Exception ElfRewritingException
