{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Definitions of the (fatal) exceptions that can be raised by Renovate
module Renovate.Core.Exception (
  RenovateException(..)
  ) where

import qualified Control.Exception as X
import qualified Data.ElfEdit as EE
import qualified Data.Macaw.CFG as MC
import qualified Prettyprinter as PP

import qualified Renovate.Core.Address as RCA
import qualified Renovate.Core.BasicBlock as RCB
import qualified Renovate.Core.Chunk as RCC

data RenovateException where
  -- | The basic block at the given address was in an invalid memory region
  -- (details in the captured 'MC.MemoryError')
  MemoryError :: (MC.MemWidth w) => MC.MemSegmentOff w -> MC.MemoryError w -> RenovateException
  -- | The basic block at the given address had no instructions in it due to the
  -- included 'X.SomeException'
  EmptyBlock :: (MC.MemWidth (MC.ArchAddrWidth arch)) => RCA.ConcreteAddress arch -> X.SomeException -> RenovateException
  -- | The memory address of the block at the given address is not wholly
  -- contained within a single byte region
  NoByteRegionAtAddress :: (MC.MemWidth w) => MC.MemAddr w -> RenovateException
  -- | The new text section size does not match the original text section size
  RewrittenTextSectionSizeMismatch :: Integer -- ^ New text section size in bytes
                                   -> Integer -- ^ Original text section size in bytes
                                   -> RenovateException
  InsufficientExtraCodeSpace :: Integer -- ^ Required bytes in the extra code section
                             -> Integer -- ^ Available bytes in the extra code section
                             -> RenovateException
  NoUnallocatedVirtualAddressSpace :: RenovateException

  -- | The given chunk was allocated at an address that is discontiguous
  DiscontiguousBlocks :: (MC.MemWidth (MC.ArchAddrWidth arch))
                      => RCC.Chunk arch
                      -> RCA.ConcreteAddress arch
                      -> RenovateException
  -- | An error was encountered while assembling a basic block
  AssemblyError :: (MC.MemWidth (MC.ArchAddrWidth arch))
                => RCB.ConcretizedBlock arch
                -> X.SomeException
                -> RenovateException
  OverlayBlockNotContained :: (MC.MemWidth (MC.ArchAddrWidth arch))
                           => RCC.Chunk arch
                           -> RCC.Chunk arch
                           -> RenovateException

  -- | Two free block allocations overlap
  OverlappingFreeBlocks :: (MC.MemWidth (MC.ArchAddrWidth arch))
                        => RCA.ConcreteAddress arch
                        -- ^ The first block address
                        -> Int
                        -- ^ The first block size
                        -> RCA.ConcreteAddress arch
                        -- ^ The second block address
                        -> Int
                        -- ^ The second block size
                        -> RenovateException

  MissingExpectedSection :: String -> RenovateException
  MultipleSectionDefinitions :: String -> Int -> RenovateException

  -- | The given architecture is not supported by renovate
  UnsupportedELFArchitecture :: EE.ElfMachine -> RenovateException
  -- | There is not enough space in the binary to fit a new PHDR section
  InsufficientPHDRSpace :: Integer -- ^ Projected offset
                        -> Integer -- ^ Required size
                        -> RenovateException
  -- | The ELF file contains no loadable segments
  NoLoadableELFSegments :: (Integral (EE.ElfWordType w)) => [EE.Phdr w] -> RenovateException
  -- | The ELF file has too many EXIDX segments (note: this is ARM-specific)
  TooManyEXIDXSegments :: [EE.SegmentIndex] -> RenovateException
  -- | The EXIDX segment was expected to have index 0, but had this index instead
  WrongEXIDXIndex :: EE.SegmentIndex -> RenovateException
  LayoutAddressOverlapsExistingSegments :: (Show (EE.ElfWordType w))
                                        => EE.ElfWordType w -- ^ Layout address
                                        -> RenovateException
  LayoutNotByteAligned :: (Show (EE.ElfWordType w))
                       => EE.ElfWordType w -- ^ Address
                       -> EE.ElfWordType w -- ^ Alignment
                       -> RenovateException
  SegmentHasRelativeSize :: Integer -> RenovateException


deriving instance Show RenovateException
instance X.Exception RenovateException

instance PP.Pretty RenovateException where
  pretty rx =
    case rx of
      MemoryError addr memErr ->
        PP.hcat [ PP.pretty "Memory error at address "
                , PP.viaShow addr
                , PP.pretty ": "
                , PP.viaShow memErr
                ]
      EmptyBlock addr x ->
        PP.hcat [ PP.pretty "Empty basic block from discovery at address "
                , PP.viaShow addr
                , PP.pretty ": "
                , PP.viaShow x
                ]
      NoByteRegionAtAddress addr ->
        PP.hsep [ PP.pretty "No byte region found in binary image at address"
                , PP.viaShow addr
                ]
      UnsupportedELFArchitecture arch ->
        PP.hsep [ PP.pretty "Unsupported architecture in the given ELF file:"
                , PP.viaShow arch
                ]
      RewrittenTextSectionSizeMismatch orig new ->
        PP.hsep [ PP.pretty "Text section size mismatch after rewriting; the original text section had"
                , PP.pretty orig
                , PP.pretty "bytes, while the rewritten text section had"
                , PP.pretty new
                , PP.pretty "bytes"
                ]
      InsufficientExtraCodeSpace required available ->
        PP.hsep [ PP.pretty "Insufficient extra code space available; the rewriter requires"
                , PP.pretty required
                , PP.pretty "bytes, but only"
                , PP.pretty available
                , PP.pretty "bytes are available"
                ]
      SegmentHasRelativeSize segNum ->
        PP.hsep [ PP.pretty "Could not compute free virtual addresses because segment"
                , PP.viaShow segNum
                , PP.pretty "has relative size"
                ]

      OverlappingFreeBlocks addr1 size1 addr2 size2 ->
        PP.hsep [ PP.pretty "Two free blocks overlap during block layout;"
                , PP.pretty size1
                , PP.pretty "bytes at"
                , PP.pretty addr1
                , PP.pretty "and"
                , PP.pretty size2
                , PP.pretty "bytes at"
                , PP.pretty addr2
                ]
      NoUnallocatedVirtualAddressSpace ->
        PP.pretty "No unallocated virtual address space within jumping range of the text section is available for use as a new extratext section."

      MissingExpectedSection secName -> PP.pretty "Missing expected section " <> PP.dquotes (PP.pretty secName)
      MultipleSectionDefinitions secName count ->
        PP.pretty "Unexpectedly found " <> PP.pretty count <> PP.pretty " instances of the " <> PP.dquotes (PP.pretty secName) <> PP.pretty " section"

      DiscontiguousBlocks cb nextAddr ->
        PP.pretty "DiscontiguousBlocks:" PP.<+> PP.pretty cb PP.<+> PP.pretty "/" PP.<+> PP.pretty nextAddr
      AssemblyError b e ->
        mconcat [ PP.pretty "Error assembling basic block at "
                , PP.pretty (RCB.concretizedBlockAddress b)
                , PP.pretty "; the error was "
                , PP.viaShow e
                ]
      OverlayBlockNotContained orig overlay ->
        PP.vsep [ PP.pretty "OverlayBlockNotContained:"
                , PP.indent 2 (PP.pretty "Base block:")
                , PP.indent 4 (PP.pretty orig)
                , PP.indent 2 (PP.pretty "Overlay block:")
                , PP.indent 4 (PP.pretty overlay)
                ]


      InsufficientPHDRSpace off sz ->
        PP.vcat [ PP.pretty "Unable to find space for a new PHDR segment"
                , PP.pretty "Offset of the PHDR segment: " <> PP.pretty off
                , PP.pretty "Size of the PHDR segment: " <> PP.pretty sz <> PP.pretty " bytes"
                ]
      NoLoadableELFSegments phdrs ->
        PP.vcat ( PP.pretty "No loadable segments:"
                : map PP.viaShow phdrs
                )
      TooManyEXIDXSegments idxs -> PP.pretty "Too many EXIDX segments: " <> PP.viaShow idxs
      WrongEXIDXIndex idx -> PP.pretty "The EXIDX segment was expected to have index 0, but had index " <> PP.pretty idx
      LayoutAddressOverlapsExistingSegments addr ->
        PP.hcat [ PP.pretty "Requested layout address"
                , PP.viaShow addr
                , PP.pretty "overlaps existing segments"
                ]
      LayoutNotByteAligned addr align ->
        PP.hcat [ PP.pretty "Requested layout address"
                , PP.viaShow addr
                , PP.pretty "is not aligned to a"
                , PP.viaShow align
                , PP.pretty "byte boundary"
                ]
