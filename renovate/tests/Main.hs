{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Test.Tasty as T
import           Test.Tasty.QuickCheck as T

import qualified Renovate.BinaryFormat.ELF.Common.Internal as Common

main :: IO ()
main = T.defaultMain $ T.testGroup "Tests" [ alignValueTests
                                           , alignValueDownTests
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
