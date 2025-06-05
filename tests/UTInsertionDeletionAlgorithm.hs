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
    [ testSparsifySimple
    , testSparsifyNegativeSingleton
    , testSparsifyComplex
    , testSparsifyLeadingNegative
    , testSparsifyOnSparsifiedRow
    , testSparsifyOnPartiallySparsifiedRow
    , testTeesZeroErrors
    , testTeesOneError
    , testMississippiZeroErrors
    , testMississippiOneError
    , testMississippiTwoErrors
    , testMississippiThreeErrors
    , testMississippiFourErrors
    , testTextEvenGapSizeZeroErrors
    , testTextEvenGapSizeOneError
    , testTextEvenGapSizeTwoErrors
    , testTextEvenGapSizeTwoErrorsBiggerInput
    , testTextEvenGapSizeThreeErrorsBiggerInput
    , testTextOddGapSizeZeroErrors
    , testTextOddGapSizeOneError
    , testTextOddGapSizeTwoErrors
    , testSmallDNAZeroErrors
    , testSmallDNAOneError
    , testDNAAsZeroErrors
    , testDNAAsOneError
    , testBigDNAZeroErrors
    , testBigDNAOneError
    , testBigDNATwoErrors
    , testBigDNAThreeErrors
    , testBigDNAFourErrors
    , testBigDNAFiveErrors
    , testDNAGapSizeOneZeroErrors
    , testDNAGapSizeOneOneError
    , testDNAGapSizeOneTwoErrors
    , testDNAGapSizeOneThreeErrors
    , testDNAGapSizeTwoZeroErrors
    , testDNAGapSizeTwoOneError
    , testDNAGapSizeTwoTwoErrors
    , testDNAGapSizeThreeZeroErrors
    , testDNAGapSizeThreeOneError
    , testDNAGapSizeThreeFourErrors
    ]

{- | Test sparsify using a simple example with a single sequence of cells with negative
budgets.
-}
testSparsifySimple :: Test
testSparsifySimple =
    TestCase $
        assertEqual
            "testSparsifySimple"
            [ Cell 0 1
            , Cell 1 0
            , Cell 2 (-1)
            , Cell 5 (-1)
            , Cell 6 0
            , -- sparsify always adds one cell with (-1) budget to the end of the row.
              Cell 7 (-1)
            ]
            ( sparsify
                [ Cell 0 1
                , Cell 1 0
                , Cell 2 (-1)
                , Cell 3 (-1)
                , Cell 4 (-1)
                , Cell 5 (-1)
                , Cell 6 0
                ]
            )

-- | Test sparsify with a sequence of cells with negative budgets of length 1.
testSparsifyNegativeSingleton :: Test
testSparsifyNegativeSingleton =
    TestCase $
        assertEqual
            "testSparsifyNegativeSingleton"
            [Cell 0 1, Cell 1 0, Cell 2 (-1), Cell 3 0, Cell 4 (-1)]
            (sparsify [Cell 0 1, Cell 1 0, Cell 2 (-1), Cell 3 0, Cell 4 (-1)])

-- | Test sparsify with a larger input and different sequences of cells with negative budgets.
testSparsifyComplex :: Test
testSparsifyComplex =
    TestCase $
        assertEqual
            "testSparsifyComplex"
            [ Cell 0 1
            , Cell 1 0
            , Cell 2 (-1)
            , Cell 5 (-1)
            , Cell 6 0
            , Cell 7 1
            , Cell 8 0
            , Cell 9 (-1)
            , Cell 10 0
            , Cell 11 0
            , Cell 12 0
            , Cell 13 (-1)
            ]
            ( sparsify
                [ Cell 0 1
                , Cell 1 0
                , Cell 2 (-1)
                , Cell 3 (-1)
                , Cell 4 (-1)
                , Cell 5 (-1)
                , Cell 6 0
                , Cell 7 1
                , Cell 8 0
                , Cell 9 (-1)
                , Cell 10 0
                , Cell 11 0
                , Cell 12 0
                , Cell 13 (-1)
                , Cell 14 (-2)
                , Cell 15 (-3)
                , Cell 16 (-4)
                ]
            )

-- | Test whether sparsify deletes the first cell if it is negative.
testSparsifyLeadingNegative :: Test
testSparsifyLeadingNegative =
    TestCase $
        assertEqual
            "testSparsifyLeadingNegative"
            [ Cell 1 0
            , Cell 2 0
            , Cell 3 (-1)
            ]
            ( sparsify
                [ Cell 0 (-1)
                , Cell 1 0
                , Cell 2 0
                , Cell 3 (-1)
                ]
            )

-- | Test whether sparsify does not change an already fully sparsified row.
testSparsifyOnSparsifiedRow :: Test
testSparsifyOnSparsifiedRow =
    TestCase $
        assertEqual
            "testSparsifyOnSparsifiedRow"
            [ Cell 0 0
            , Cell 1 (-1)
            , Cell 10 (-1)
            , Cell 11 0
            , Cell 12 (-1)
            ]
            ( sparsify
                [ Cell 0 0
                , Cell 1 (-1)
                , Cell 10 (-1)
                , Cell 11 0
                , Cell 12 (-1)
                ]
            )

-- | Test sparsify on a row which has previously already been partially sparsified.
testSparsifyOnPartiallySparsifiedRow :: Test
testSparsifyOnPartiallySparsifiedRow =
    TestCase $
        assertEqual
            "testSparsifyOnPartiallySparsifiedRow"
            [ Cell 0 0
            , Cell 1 (-1)
            , Cell 5 (-1)
            , Cell 6 0
            , Cell 7 (-1)
            , Cell 9 (-1)
            , Cell 10 0
            , Cell 11 (-1)
            ]
            ( sparsify
                [ Cell 0 0
                , Cell 1 (-1)
                , Cell 5 (-1)
                , Cell 6 0
                , Cell 7 (-1)
                , Cell 8 (-1)
                , Cell 9 (-1)
                , Cell 10 0
                , Cell 11 (-1)
                ]
            )

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
            (sort $ insertionDeletionAlgorithm 0 3 (V.fromList "mississippi"))

-- | Test the string "mississippi" with four errors.
testMississippiFourErrors :: Test
testMississippiFourErrors =
    TestCase $
        assertEqual
            "testMississippiFourErrors"
            -- The whole string is an approximate palindrome with three errors
            [(0, 11)]
            (sort $ insertionDeletionAlgorithm 0 4 (V.fromList "mississippi"))

-- | Test text input with a non-zero even gapSize and no errors.
testTextEvenGapSizeZeroErrors :: Test
testTextEvenGapSizeZeroErrors =
    TestCase $
        assertEqual
            "testTextEvenGapSizeZeroErrors"
            [(0, 2), (0, 6), (1, 3), (3, 5), (3, 7), (5, 7)]
            (sort $ insertionDeletionAlgorithm 2 0 (V.fromList "dabcadc"))

-- | Test text input with a non-zero even gapSize and one error.
testTextEvenGapSizeOneError :: Test
testTextEvenGapSizeOneError =
    TestCase $
        assertEqual
            "testTextEvenGapSizeOneError"
            [(0, 7), (2, 7)]
            (sort $ insertionDeletionAlgorithm 2 1 (V.fromList "dabcadc"))

{- | Test text input with a non-zero even gapSize and two errors. The whole string is an
approximate palindrome with one error and there are no substrings which are gapped
maximal palindromes with 2 errors, so this should return the whole string.
-}
testTextEvenGapSizeTwoErrors :: Test
testTextEvenGapSizeTwoErrors =
    TestCase $
        assertEqual
            "testTextEvenGapSizeTwoErrors"
            [(0, 7)]
            (sort $ insertionDeletionAlgorithm 2 2 (V.fromList "dabcadc"))

{- | Test again with an even gapSize, but with bigger input with more interesting output when
searching with a maximum of 2 errors.
-}
testTextEvenGapSizeTwoErrorsBiggerInput :: Test
testTextEvenGapSizeTwoErrorsBiggerInput =
    TestCase $
        assertEqual
            "testTextEvenGapSizeTwoErrorsBiggerInput"
            [(0, 9), (2, 10)]
            (sort $ insertionDeletionAlgorithm 2 2 (V.fromList "cbadcabede"))

{- | Use the same gapSize and input as "testTextEvenGapSizeTwoErrorsBiggerInput", but search for
maximum three errors. Should return the whole string.
-}
testTextEvenGapSizeThreeErrorsBiggerInput :: Test
testTextEvenGapSizeThreeErrorsBiggerInput =
    TestCase $
        assertEqual
            "testTextEvenGapSizeThreeErrorsBiggerInput"
            [(0, 10)]
            (sort $ insertionDeletionAlgorithm 2 3 (V.fromList "cbadcabede"))

{- | Test with an odd gapSize and zero errors. The gapSize is 3 and not 1, because 1 character
is always a palindrome because of reflexitivity, so a gapSize of size 1 does not change the
results.
-}
testTextOddGapSizeZeroErrors :: Test
testTextOddGapSizeZeroErrors =
    TestCase $
        assertEqual
            "testTextOddGapSizeZeroErrors"
            [(0, 3), (1, 5), (1, 7), (3, 8), (5, 8), (6, 9), (7, 10)]
            (sort $ insertionDeletionAlgorithm 3 0 (V.fromList "cbdabdbacc"))

-- | Test with an odd gapSize and one error.
testTextOddGapSizeOneError :: Test
testTextOddGapSizeOneError =
    TestCase $
        assertEqual
            "testTextOddGapSizeOneError"
            [(0, 9), (2, 9), (5, 10)]
            (sort $ insertionDeletionAlgorithm 3 1 (V.fromList "cbdabdbacc"))

{- | Test with an odd gapSize and two errors. The only maximal gapped approximate palindrome
satisfying these constraints is the whole string.
-}
testTextOddGapSizeTwoErrors :: Test
testTextOddGapSizeTwoErrors =
    TestCase $
        assertEqual
            "testTextOddGapSizeTwoErrors"
            [(0, 10)]
            (sort $ insertionDeletionAlgorithm 3 2 (V.fromList "cbdabdbacc"))

{- | Test a small DNA sequence with zero errors. Note that for an empty maximal
approximate palindrome, the start character index is the same as the end character index.
Both are the index of the character directly following the empty string in question.
-}
testSmallDNAZeroErrors :: Test
testSmallDNAZeroErrors =
    TestCase $
        assertEqual
            "testSmallDNAZeroErrors"
            [(0, 2), (2, 2), (3, 3), (4, 4)]
            (sort $ insertionDeletionAlgorithm 0 0 (V.fromList [A, T, G, G]))

{- | Test the small DNA sequence with one error. The output represents strings [A, T, G]
and [G, G].
-}
testSmallDNAOneError :: Test
testSmallDNAOneError =
    TestCase $
        assertEqual
            "testSmallDNAOneError"
            [(0, 3), (2, 4)]
            (sort $ insertionDeletionAlgorithm 0 1 (V.fromList [A, T, G, G]))

-- | test AAAAAAAAA with no errors
testDNAAsZeroErrors :: Test
testDNAAsZeroErrors =
    TestCase $
        assertEqual
            "testDNAAsZeroErrors"
            [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (8, 8), (9, 9)]
            (sort $ insertionDeletionAlgorithm 0 0 (V.fromList [A, A, A, A, A, A, A, A, A]))

-- | test AAAAAAAAA with one errors
testDNAAsOneError :: Test
testDNAAsOneError =
    TestCase $
        assertEqual
            "testDNAAsOneError"
            [(0, 2), (1, 3), (2, 4), (3, 5), (4, 6), (5, 7), (6, 8), (7, 9)]
            (sort $ insertionDeletionAlgorithm 0 1 (V.fromList [A, A, A, A, A, A, A, A, A]))

{- | Test a big (at least bigger than the small DNA sequence) DNA sequence with zero
errors.
-}
testBigDNAZeroErrors :: Test
testBigDNAZeroErrors =
    TestCase $
        assertEqual
            "testBigDNAZeroErrors"
            [(1, 1), (1, 3), (3, 3), (4, 4), (5, 5), (6, 6), (6, 8), (8, 8), (9, 9), (10, 10)]
            (sort $ insertionDeletionAlgorithm 0 0 (V.fromList [A, G, C, A, A, G, T, A, A, C]))

{- | Test a big (at least bigger than the small DNA sequence) DNA sequence with one
error.
-}
testBigDNAOneError :: Test
testBigDNAOneError =
    TestCase $
        assertEqual
            "testBigDNAOneError"
            [(0, 4), (2, 6), (3, 7), (5, 10), (8, 10)]
            (sort $ insertionDeletionAlgorithm 0 1 (V.fromList [A, G, C, A, A, G, T, A, A, C]))

{- | Test a big (at least bigger than the small DNA sequence) DNA sequence with two
errors.
-}
testBigDNATwoErrors :: Test
testBigDNATwoErrors =
    TestCase $
        assertEqual
            "testBigDNATwoErrors"
            [(0, 5), (0, 7), (2, 8), (4, 10)]
            (sort $ insertionDeletionAlgorithm 0 2 (V.fromList [A, G, C, A, A, G, T, A, A, C]))

{- | Test a big (at least bigger than the small DNA sequence) DNA sequence with three
errors.
-}
testBigDNAThreeErrors :: Test
testBigDNAThreeErrors =
    TestCase $
        assertEqual
            "testBigDNAThreeErrors"
            [(0, 8), (1, 10), (3, 10)]
            (sort $ insertionDeletionAlgorithm 0 3 (V.fromList [A, G, C, A, A, G, T, A, A, C]))

{- | Test a big (at least bigger than the small DNA sequence) DNA sequence with four
errors. The whole string is then an approximate palindrome.
-}
testBigDNAFourErrors :: Test
testBigDNAFourErrors =
    TestCase $
        assertEqual
            "testBigDNAFourErrors"
            [(0, 10)]
            (sort $ insertionDeletionAlgorithm 0 4 (V.fromList [A, G, C, A, A, G, T, A, A, C]))

{- | Test a big (at least bigger than the small DNA sequence) DNA sequence with five
errors. The whole string is then an approximate palindrome. This is tested in addition to
testing with four errors to assert that maximal approximate palindromes with strictly
fewer errors than the maximum are also found.
-}
testBigDNAFiveErrors :: Test
testBigDNAFiveErrors =
    TestCase $
        assertEqual
            "testBigDNAFiveErrors"
            [(0, 10)]
            (sort $ insertionDeletionAlgorithm 0 5 (V.fromList [A, G, C, A, A, G, T, A, A, C]))

{- | Test some DNA string input with a gapSize of 1 and 0 errors. This is the first
test for a non-zero gapSize.
-}
testDNAGapSizeOneZeroErrors :: Test
testDNAGapSizeOneZeroErrors =
    TestCase $
        assertEqual
            "testDNAGapSizeOneZeroErrors"
            [(0, 1), (0, 3), (1, 4), (3, 6), (5, 6), (6, 7)]
            (sort $ insertionDeletionAlgorithm 1 0 (V.fromList [A, C, T, G, C, C, T]))

{- | Test some DNA string input with a gapSize of 1 and 1 error. This tests the
combination of gapSize and errors.
-}
testDNAGapSizeOneOneError :: Test
testDNAGapSizeOneOneError =
    TestCase $
        assertEqual
            "testDNAGapSizeOneOneError"
            [(0, 5), (2, 7)]
            (sort $ insertionDeletionAlgorithm 1 1 (V.fromList [A, C, T, G, C, C, T]))

{- | Test some DNA string input with a gapSize of 1 and 2 errors. The output should be
the indices for the whole string.
-}
testDNAGapSizeOneTwoErrors :: Test
testDNAGapSizeOneTwoErrors =
    TestCase $
        assertEqual
            "testDNAGapSizeOneTwoErrors"
            [(0, 7)]
            (sort $ insertionDeletionAlgorithm 1 2 (V.fromList [A, C, T, G, C, C, T]))

{- | Test some DNA string input with a gapSize of 1 and 3 errors. The output should be
the indices for the whole string. This tests whether gapped approximate strings with less errors
than the maximum are found correctly.
-}
testDNAGapSizeOneThreeErrors :: Test
testDNAGapSizeOneThreeErrors =
    TestCase $
        assertEqual
            "testDNAGapSizeOneThreeErrors"
            [(0, 7)]
            (sort $ insertionDeletionAlgorithm 1 3 (V.fromList [A, C, T, G, C, C, T]))

{- | Test some DNA string input with a gapSize of 2 and 0 errors. This tests the
algorithm using an even gap.
-}
testDNAGapSizeTwoZeroErrors :: Test
testDNAGapSizeTwoZeroErrors =
    TestCase $
        assertEqual
            "testDNAGapSizeTwoZeroErrors"
            [(0, 2), (0, 4), (1, 6), (4, 7), (6, 8)]
            (sort $ insertionDeletionAlgorithm 2 0 (V.fromList [A, G, G, T, C, C, G, T]))

-- | Test some DNA string input with a gapSize of 2 and 1 error. Tests even gapSize with an error.
testDNAGapSizeTwoOneError :: Test
testDNAGapSizeTwoOneError =
    TestCase $
        assertEqual
            "testDNAGapSizeTwoZeroErrors"
            [(0, 8), (3, 8)]
            (sort $ insertionDeletionAlgorithm 2 1 (V.fromList [A, G, G, T, C, C, G, T]))

{- | Test some DNA string input with a gapSize of 2 and 2 errors. Tests even gapSize with an
error and tests whether the algorithm makes a jump from having two errors to having one
error again by adding a single character.
-}
testDNAGapSizeTwoTwoErrors :: Test
testDNAGapSizeTwoTwoErrors =
    TestCase $
        assertEqual
            "testDNAGapSizeTwoTwoErrors"
            [(0, 8)]
            (sort $ insertionDeletionAlgorithm 2 2 (V.fromList [A, G, G, T, C, C, G, T]))

{- | Test some DNA string input with a gapSize of 3 and 0 errors. Tests an odd gapSize which
is bigger than 1.
-}
testDNAGapSizeThreeZeroErrors :: Test
testDNAGapSizeThreeZeroErrors =
    TestCase $
        assertEqual
            "testDNAGapSizeThreeZeroErrors"
            [(0, 3), (1, 4), (1, 6), (1, 8), (5, 8)]
            (sort $ insertionDeletionAlgorithm 3 0 (V.fromList [C, C, T, T, A, G, A, G]))

{- | Test some DNA string input with a gapSize of 3 and 1 error. Tests an odd gapSize which
is bigger than 1 in combination with an error.
-}
testDNAGapSizeThreeOneError :: Test
testDNAGapSizeThreeOneError =
    TestCase $
        assertEqual
            "testDNAGapSizeThreeOneError"
            [(0, 8)]
            (sort $ insertionDeletionAlgorithm 3 1 (V.fromList [C, C, T, T, A, G, A, G]))

{- | Test some DNA string input with a gapSize of 3 and 4 errors. Tests an odd gapSize which
is bigger than 1 in combination with an error and tests whether the algorithm finds that
the whole string is an approximate palindrome with less than 4 errors.
-}
testDNAGapSizeThreeFourErrors :: Test
testDNAGapSizeThreeFourErrors =
    TestCase $
        assertEqual
            "testDNAGapSizeThreeFourErrors"
            [(0, 8)]
            (sort $ insertionDeletionAlgorithm 3 4 (V.fromList [C, C, T, T, A, G, A, G]))
