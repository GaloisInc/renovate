{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.List.NonEmpty as NEL

import qualified Test.Tasty as T
import           Test.Tasty.HUnit as T
import           Test.Tasty.QuickCheck as T

import qualified Renovate.BinaryFormat.ELF.Internal as ELF
import qualified Renovate.BinaryFormat.ELF.Common.Internal as Common

main :: IO ()
main = T.defaultMain $ T.testGroup "Tests" [ alignValueTests
                                           , alignValueDownTests
                                           , findSpaceForPHDRsTests
                                           ]

mkAlignValueTests :: (Int -> Int -> Int) -> [T.TestTree]
mkAlignValueTests align =
  [ T.testProperty "Returns the value when the alignment is 1" $
    \v -> align v 1 T.=== v
  , T.testProperty "Returns the value when the alignment is 0" $
    \v -> align v 0 T.=== v
  , T.testProperty "Returns the value when it's already aligned" $
    \v n -> align (n * v) n T.=== (n * v)
  , T.testProperty "Returns an aligned value" $
    \v n -> n /= 0 T.==> align v n `mod` n T.=== 0
  ]

alignValueTests :: T.TestTree
alignValueTests =
  T.testGroup "alignValueTests" $ mkAlignValueTests Common.alignValue ++
    [ T.testProperty "Returns a >= value" $
       \(v :: Int) (n :: Int) ->
         n >= 0 T.==> (Common.alignValue v n >= v) T.=== True
    ]

alignValueDownTests :: T.TestTree
alignValueDownTests =
  T.testGroup "alignValueDownTests" $ mkAlignValueTests Common.alignValueDown ++
    [ T.testProperty "Returns a <= value" $
       \(v :: Int) (n :: Int) ->
         n >= 0 T.==> (Common.alignValueDown v n <= v) T.=== True
    ]

findSpaceForPHDRsTests :: T.TestTree
findSpaceForPHDRsTests =
  T.testGroup "findSpaceForPHDRsTests" $
    [ T.testCase "Can't get closer than optimal" $
        Nothing T.@=?
          let info :: ELF.LoadSegmentInfo 64
              info = ELF.LoadSegmentInfo
                       { ELF.pOffset = someELFWord
                       , ELF.pVAddr = someELFWord
                       , ELF.pMemSz = someELFWord
                       }
          in ELF.findSpaceForPHDRs (info NEL.:| []) someELFWord someELFWord
    , T.testCase "Before all other segments" $
        Just 0x1ff000 T.@=?
          let info1 :: ELF.LoadSegmentInfo 64
              info1 = ELF.LoadSegmentInfo
                        { ELF.pOffset = someELFWord
                        , ELF.pVAddr = 3 * someELFWord
                        , ELF.pMemSz = someELFWord
                        }
              info2 :: ELF.LoadSegmentInfo 64
              info2 = ELF.LoadSegmentInfo
                        { ELF.pOffset = 3 * someELFWord
                        , ELF.pVAddr = 5 * someELFWord
                        , ELF.pMemSz = someELFWord
                        }
          in ELF.findSpaceForPHDRs (info1 NEL.:| [info2]) someELFWord someELFWord
    -- Values in the following test case are taken from a real binary
    , T.testCase "After all other segments (high offset)" $
        -- Type           Offset   VirtAddr   PhysAddr   FileSiz MemSiz  Flg Align
        -- LOAD           0x000000 0x00010000 0x00010000 0x65de8 0x65de8 R E 0x10000
        -- LOAD           0x066b7c 0x00086b7c 0x00086b7c 0x01364 0x02374 RW  0x10000
        let offset = 0xA3000
        in
          Just offset T.@=?
            let info1 :: ELF.LoadSegmentInfo 64
                info1 = ELF.LoadSegmentInfo
                          { ELF.pOffset = 0x0
                          , ELF.pVAddr = 0x10000
                          , ELF.pMemSz = 0x65de8
                          }
                info2 :: ELF.LoadSegmentInfo 64
                info2 = ELF.LoadSegmentInfo
                          { ELF.pOffset = 0x66b7c
                          , ELF.pVAddr = 0x86b7c
                          , ELF.pMemSz = 0x02374
                          }
            in ELF.findSpaceForPHDRs (info1 NEL.:| [info2]) offset 0x160
    -- Values in the following test case are taken from
    -- linked-list.noopt.nostdlib.aarch32.exe. Unfortunately, it is
    -- unsatisfiable because the .bss section has a HUGE buffer in it, which
    -- overlaps with where we want to put the new PHDRs.
    , T.testCase "linked-list.noopt.nostdlib.aarch32.exe" $
        -- Type           Offset   VirtAddr   PhysAddr   FileSiz MemSiz   Flg Align
        -- LOAD           0x000000 0x00010000 0x00010000 0x0031c 0x0031c  R E 0x10000
        -- LOAD           0x00031c 0x0002031c 0x0002031c 0x00004 0x400008 RW  0x10000
        Nothing T.@=?
          let info1 :: ELF.LoadSegmentInfo 64
              info1 = ELF.LoadSegmentInfo
                        { ELF.pOffset = 0x0
                        , ELF.pVAddr = 0x10000
                        , ELF.pMemSz = 0x0031c
                        }
              info2 :: ELF.LoadSegmentInfo 64
              info2 = ELF.LoadSegmentInfo
                        { ELF.pOffset = 0x0031c
                        , ELF.pVAddr = 0x2031c
                        , ELF.pMemSz = 0x400008
                        }
          in ELF.findSpaceForPHDRs (info1 NEL.:| [info2]) 0x404000 0x100
    , T.testCase "test-just-exit.nostdlib.ppc64.exe" $
        -- LOAD 0x000000000 0x010000000 0x010000000 0x0000b2a11 0x0000b2a11  R E    0x10000
        -- LOAD 0x0000b9c80 0x0100c9c80 0x0100c9c80 0x000008448 0x000009aa8  RW     0x10000
        Just 0xffff000 T.@=?
          let info1 :: ELF.LoadSegmentInfo 64
              info1 = ELF.LoadSegmentInfo
                        { ELF.pOffset = 0x0
                        , ELF.pVAddr = 0x010000000
                        , ELF.pMemSz = 0x0000b2a11
                        }
              info2 :: ELF.LoadSegmentInfo 64
              info2 = ELF.LoadSegmentInfo
                        { ELF.pOffset = 0x0000b9c80
                        , ELF.pVAddr = 0x0100c9c80
                        , ELF.pMemSz = 0x000009aa8
                        }
          in ELF.findSpaceForPHDRs (info1 NEL.:| [info2]) 0x09aa8 0x230
    ]
  where someELFWord = 0x100000

