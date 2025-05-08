module UTInsertionDeletionAlgorithm (testListInsertionDeletionAlgorithm) where

import Data.List (sort)
import Test.HUnit (Test (..), assertEqual)

import Data.Algorithms.Palindromes.DNA (DNA (..))
import Data.Algorithms.Palindromes.InsertionDeletionAlgorithm
    ( Cell (..)
    , insertionDeletionAlgorithm
    )

import qualified Data.Vector as V

testListInsertionDeletionAlgorithm =
    [ testTeesZeroErrors
    , testTeesOneError
    , testMississippiZeroErrors
    , testMississippiOneError
    , testMississippiTwoErrors
    , testMississippiThreeErrors
    , testMississippiFourErrors
    ]

{- | This tests the input string "tees" with zero errors. Output order does not matter, so
it is sorted to check for equality. Output represents the strings "t", "ee" and "s".
-}
testTeesZeroErrors :: Test
testTeesZeroErrors =
    TestCase $
        assertEqual
            "testTeesZeroErrors"
            [(0, 1), (1, 3), (3, 4)]
            (sort $ insertionDeletionAlgorithm 0 (V.fromList "tees"))

{- | This tests the input string "tees" with one error. The entire string is then one
maximal approximate palindrome.
-}
testTeesOneError :: Test
testTeesOneError =
    TestCase $
        assertEqual
            "testTeesOneError"
            [(0, 4)]
            (sort $ insertionDeletionAlgorithm 1 (V.fromList "tees"))

-- | Test the string "mississippi" with zero errors.
testMississippiZeroErrors :: Test
testMississippiZeroErrors =
    TestCase $
        assertEqual
            "testMississippiZeroErrors"
            [ (0, 1) -- "m"
            , (1, 2) -- "i"
            , (1, 5) -- "issi"
            , (1, 8) -- "ississi"
            , (4, 8) -- "issi"
            , (7, 8) -- "i"
            , (7, 11) -- "ippi"
            , (10, 11) -- "i"
            ]
            (sort $ insertionDeletionAlgorithm 0 (V.fromList "mississippi"))

-- | Test the string "mississippi" with one error.
testMississippiOneError :: Test
testMississippiOneError =
    TestCase $
        assertEqual
            "testMississippiOneError"
            [ (0, 3) -- "mis"
            , (0, 6) -- "missis"
            , (0, 9) -- "mississip"
            , (3, 9) -- "sissip"
            , (6, 9) -- "sip"
            , (6, 11) -- "sippi"
            ]
            (sort $ insertionDeletionAlgorithm 1 (V.fromList "mississippi"))

-- | Test the string "mississippi" with two errors.
testMississippiTwoErrors :: Test
testMississippiTwoErrors =
    TestCase $
        assertEqual
            "testMississippiTwoErrors"
            [ (0, 10) -- "mississipp"
            , (1, 11) -- "ississippi"
            , (4, 11) -- "issippi"
            ]
            (sort $ insertionDeletionAlgorithm 2 (V.fromList "mississippi"))

-- | Test the string "mississippi" with three errors.
testMississippiThreeErrors :: Test
testMississippiThreeErrors =
    TestCase $
        assertEqual
            "testMississippiThreeErrors"
            -- The whole string is an approximate palindrome with three errors
            [(0, 11)]
            (insertionDeletionAlgorithm 3 (V.fromList "mississippi"))

-- | Test the string "mississippi" with four errors.
testMississippiFourErrors :: Test
testMississippiFourErrors =
    TestCase $
        assertEqual
            "testMississippiFourErrors"
            -- The whole string is an approximate palindrome with three errors
            [(0, 11)]
            (insertionDeletionAlgorithm 4 (V.fromList "mississippi"))
