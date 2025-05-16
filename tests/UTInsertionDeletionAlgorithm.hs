module UTInsertionDeletionAlgorithm (testListInsertionDeletionAlgorithm) where

import Data.List (sort)
import Test.HUnit (Test (..), assertEqual)

import Data.Algorithms.Palindromes.DNA (DNA (..))
import Data.Algorithms.Palindromes.InsertionDeletionAlgorithm
    ( Cell (..)
    , insertionDeletionAlgorithm
    , sparsify
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
    , -- , testSparsifySimple
      -- , testSparsifyEdgeCase
      -- , testSparsifyComplex
      testDNAZeroErrors
    , testDNAOneError
    , testDNAAsZeroErrors
    , testDNAAsOneError
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
            (sort $ insertionDeletionAlgorithm 0 0 (V.fromList "tees"))

{- | This tests the input string "tees" with one error. The entire string is then one
maximal approximate palindrome.
-}
testTeesOneError :: Test
testTeesOneError =
    TestCase $
        assertEqual
            "testTeesOneError"
            [(0, 4)]
            (sort $ insertionDeletionAlgorithm 0 1 (V.fromList "tees"))

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
            (sort $ insertionDeletionAlgorithm 0 0 (V.fromList "mississippi"))

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
            (sort $ insertionDeletionAlgorithm 0 1 (V.fromList "mississippi"))

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
            (sort $ insertionDeletionAlgorithm 0 2 (V.fromList "mississippi"))

-- | Test the string "mississippi" with three errors.
testMississippiThreeErrors :: Test
testMississippiThreeErrors =
    TestCase $
        assertEqual
            "testMississippiThreeErrors"
            -- The whole string is an approximate palindrome with three errors
            [(0, 11)]
            (insertionDeletionAlgorithm 0 3 (V.fromList "mississippi"))

-- | Test the string "mississippi" with four errors.
testMississippiFourErrors :: Test
testMississippiFourErrors =
    TestCase $
        assertEqual
            "testMississippiFourErrors"
            -- The whole string is an approximate palindrome with three errors
            [(0, 11)]
            (insertionDeletionAlgorithm 0 4 (V.fromList "mississippi"))

-- testSparsifySimple :: Test
-- testSparsifySimple =
--     TestCase $
--         assertEqual
--             "testSparsifySimple"
--             [Cell (0, 0) 1, Cell (0, 1) 0, Cell (0, 2) (-1), Cell (0, 5) (-1), Cell (0, 6) 0]
--             ( sparsify
--                 7
--                 [ Cell (0, 0) 1
--                 , Cell (0, 1) 0
--                 , Cell (0, 2) (-1)
--                 , Cell (0, 3) (-1)
--                 , Cell (0, 4) (-1)
--                 , Cell (0, 5) (-1)
--                 , Cell (0, 6) 0
--                 ]
--             )

-- testSparsifyEdgeCase :: Test
-- testSparsifyEdgeCase =
--     TestCase $
--         assertEqual
--             "testSparsifyEdgeCase"
--             [Cell (0, 0) 1, Cell (0, 1) 0, Cell (0, 2) (-1), Cell (0, 3) 0]
--             (sparsify 4 [Cell (0, 0) 1, Cell (0, 1) 0, Cell (0, 2) (-1), Cell (0, 3) 0])

-- testSparsifyComplex :: Test
-- testSparsifyComplex =
--     TestCase $
--         assertEqual
--             "testSparsifyComplex"
--             [ Cell (0, 0) 1
--             , Cell (0, 1) 0
--             , Cell (0, 2) (-1)
--             , Cell (0, 5) (-1)
--             , Cell (0, 6) 0
--             , Cell (0, 7) 1
--             , Cell (0, 8) 0
--             , Cell (0, 9) (-1)
--             , Cell (0, 10) 0
--             , Cell (0, 11) 0
--             , Cell (0, 12) 0
--             , Cell (0, 13) (-1)
--             ]
--             ( sparsify
--                 17
--                 [ Cell (0, 0) 1
--                 , Cell (0, 1) 0
--                 , Cell (0, 2) (-1)
--                 , Cell (0, 3) (-1)
--                 , Cell (0, 4) (-1)
--                 , Cell (0, 5) (-1)
--                 , Cell (0, 6) 0
--                 , Cell (0, 7) 1
--                 , Cell (0, 8) 0
--                 , Cell (0, 9) (-1)
--                 , Cell (0, 10) 0
--                 , Cell (0, 11) 0
--                 , Cell (0, 12) 0
--                 , Cell (0, 13) (-1)
--                 , Cell (0, 14) (-2)
--                 , Cell (0, 15) (-3)
--                 , Cell (0, 16) (-4)
--                 ]
--             )

{- | Test a small DNA sequence with zero errors. Note that for an empty maximal
approximate palindrome, the start character index is the same as the end character index.
Both are the index of the character directly following the empty string in question.
-}
testDNAZeroErrors :: Test
testDNAZeroErrors =
    TestCase $
        assertEqual
            "testDNAZeroErrors"
            [(0, 2), (2, 2), (3, 3)]
            (sort $ insertionDeletionAlgorithm 0 0 (V.fromList [A, T, G, G]))

{- | Test the small DNA sequence with one error. The output represents strings [A, T, G]
and [G, G].
-}
testDNAOneError :: Test
testDNAOneError =
    TestCase $
        assertEqual
            "testDNAOneError"
            [(0, 3), (2, 4)]
            (sort $ insertionDeletionAlgorithm 0 1 (V.fromList [A, T, G, G]))
{- | test AAAAAAAAA with no errors-}
testDNAAsZeroErrors :: Test
testDNAAsZeroErrors =
    TestCase $
        assertEqual
            "testDNAAsZeroErrors"
            [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)]
            (sort $ insertionDeletionAlgorithm 0 0 (V.fromList [A,A,A,A,A,A,A,A,A]))
{- | test AAAAAAAAA with one errors-}
testDNAAsOneError :: Test
testDNAAsOneError =
    TestCase $
        assertEqual
            "testDNAAsOneError"
            [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9)]
            (sort $ insertionDeletionAlgorithm 1 0 (V.fromList [A,A,A,A,A,A,A,A,A]))