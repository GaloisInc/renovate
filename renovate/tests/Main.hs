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
    ]
  where someELFWord = 0x100000
