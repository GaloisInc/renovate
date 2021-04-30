{-# LANGUAGE GADTs #-}

-- | Utilities for working with user-space QEMU.

module Refurbish.QEMU
  (
    qemulator
  )
where

import qualified Data.ByteString as BS
import qualified Data.ElfEdit as Elf
import           Data.Maybe ( fromMaybe )


-- | Determines the proper QEMU user-space emulator to run for the
-- specified executable file.

qemulator :: FilePath -> IO String
qemulator exe =
  do bytes <- BS.readFile exe
     case Elf.decodeElfHeaderInfo bytes of
       Left (byteOff, msg) -> do
         error ("Error decoding target execution ELF file"
                <> " at " <> show byteOff <> ": " <> msg)
       Right (Elf.SomeElf e0) ->
         let hdr = Elf.header e0
             qemus = [ (Elf.EM_PPC,    "qemu-ppc")
                     , (Elf.EM_PPC64,  "qemu-ppc64")
                     , (Elf.EM_X86_64, "qemu-x86_64")
                     , (Elf.EM_ARM,    "qemu-arm")
                     ]
             no_qemu = error ("Unsupported target execution ELF type: "
                              <> show (Elf.headerMachine hdr) <> " "
                              <> show (Elf.headerClass hdr))
         in return $ fromMaybe no_qemu $ lookup (Elf.headerMachine hdr) qemus
