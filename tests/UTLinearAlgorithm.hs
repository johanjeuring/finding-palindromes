{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}

module UTLinearAlgorithm (testListLinearAlgorithm) where

import Test.HUnit (Test (..), assertEqual)

import Data.Algorithms.Palindromes.DNA (DNA (..), toDNA)

import qualified Data.Vector as V

import qualified Data.Algorithms.Palindromes.LinearAlgorithm as P

testListLinearAlgorithm =
    [ testExtendPalindromeSSimple
    , testExtendPalindromeSWhole
    , testExtendPalindromeSIntertwined
    , testExtendPalindromeSNothing
    , testExtendPalindromeSEmpty
    , testExtendPalindromeSDNA
    , testExtendPalindromeSDNAAllCenters
    , testMoveCenterSSimple
    , testMoveCenterSGuard1
    , testMoveCenterSGuard2
    , testMoveCenterSGuard3
    , testMoveCenterSDNA
    , testMoveCenterSDNAAllCenters
    , testFinalPalindromesSSimple
    , testFinalPalindromesSCutOff
    , testFinalPalindromesNrOfCentersZero
    , testFinalPalindromesSDNA
    , testFinalPalindromesSDNACutOff
    ]

{-
----------------------------------------------------------
    Begin tests for extendPalindromeS
----------------------------------------------------------
-}
{- Test a simple small string with extendPalindromeS -}
testExtendPalindromeSSimple =
    TestCase $
        assertEqual
            "testExtendPalindromeSSimple"
            [0, 1, 0, 3, 0, 1, 0, 1, 2, 1, 0]
            ( P.extendPalindromeS
                False
                (V.fromList "nnaba")
                0
                []
                0
            )

{- Test a string on extendPalindromeS consisting of one big palindrome. The output list should be symmetrical -}
testExtendPalindromeSWhole =
    TestCase $
        assertEqual
            "testExtendPalindromeSWhole"
            [0, 1, 0, 1, 2, 1, 0, 1, 0, 1, 0, 11, 0, 1, 0, 1, 0, 1, 2, 1, 0, 1, 0]
            ( P.extendPalindromeS
                False
                (V.fromList "meetsysteem")
                0
                []
                0
            )

{- Test a string on extendPalindromeS where two of the oalindromes overlap with each other, in this case "leepeel"
and "peeleep" -}
testExtendPalindromeSIntertwined =
    TestCase $
        assertEqual
            "testExtendPalindromeSIntertwined"
            [0, 1, 0, 1, 2, 1, 0, 7, 0, 1, 2, 1, 0, 7, 0, 1, 2, 1, 0, 1, 0]
            ( P.extendPalindromeS
                False
                (V.fromList "leepeeleep")
                0
                []
                0
            )

{- Test a string on extendPalindromeS not containing any palindromes -}
testExtendPalindromeSNothing =
    TestCase $
        assertEqual
            "testExtendPalindromeSNothing"
            [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]
            ( P.extendPalindromeS
                False
                (V.fromList "abcdefgh")
                0
                []
                0
            )

{- Test an empty string on extendPalindromeS -}
testExtendPalindromeSEmpty =
    TestCase $
        assertEqual
            "testExtendPalindromeSEmpty"
            [0]
            ( P.extendPalindromeS
                False
                (V.fromList "")
                0
                []
                0
            )

-- Test a DNA string on extendPalindromeS
testExtendPalindromeSDNA =
    TestCase $
        assertEqual
            "testExtendPalindromeSDNA"
            [0, 2, 0, 6, 0, 2, 0]
            ( P.extendPalindromeS
                True
                (V.fromList [A, T, G, C, A, T])
                0
                []
                0
            )

{- Test a DNA string on extendPalindromeS without skipping the centers on elements. If
this works, non-reflexive, non-anti-reflexive datatypes will likely also work -}
testExtendPalindromeSDNAAllCenters =
    TestCase $
        assertEqual
            "testExtendPalindromeSDNAAllCenters"
            [0, 0, 2, 0, 4, 0, 2, 0, 0, 0, 2, 0, 0]
            ( P.extendPalindromeS
                False
                (V.fromList [A, T, G, C, G, C])
                0
                []
                0
            )

{-
----------------------------------------------------------
    End tests for extendPalindromeS
----------------------------------------------------------
-}

{-
----------------------------------------------------------
    Begin tests for moveCenterS
----------------------------------------------------------
-}

{- Test moveCenterS on a simple string -}
testMoveCenterSSimple =
    TestCase $
        assertEqual
            "testMoveCenterSSimple"
            [0, 1, 0, 1, 4, 1, 0, 1, 4, 1, 0, 3, 0, 1, 0]
            ( P.moveCenterS
                False
                (V.fromList "abaabba")
                5
                [4, 1, 0, 3, 0, 1, 0]
                [1, 0, 3, 0, 1, 0]
                4
            )

{- Tests moveCenterS on a string, ensuring it passes through
the first guard of moveCenterS by making rightmost = 0 -}
testMoveCenterSGuard1 =
    TestCase $
        assertEqual
            "testMoveCenterSGuard1"
            [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]
            ( P.moveCenterS
                False
                (V.fromList "abcdefgh")
                4
                [0, 1, 0, 1, 0, 1, 0, 1, 0]
                [1, 0, 1, 0, 1, 0, 1, 0]
                0
            )

{- Tests moveCenterS on a string, ensuring it passes through
the second guard of moveCenterS by ensuring
head maximalPalindromesIn' == nrOfCenters - centerfactor -}
testMoveCenterSGuard2 =
    TestCase $
        assertEqual
            "testMoveCenterSGuard2"
            [0, 1, 0, 1, 0, 1, 0, 1, 2, 3, 4, 3, 2, 1, 0, 1, 0, 1, 0, 1, 0]
            ( P.moveCenterS
                False
                (V.fromList "abcaaaaxyz")
                7
                [4, 3, 2, 1, 0, 1, 0, 1, 0, 1, 0]
                [3, 2, 1, 0, 1, 0, 1, 0, 1, 0]
                4
            )

{- Tests moveCenterS on a string, ensuring it passes through
the third guard of moveCenterS by ensuring neither
rightmost = 0 and head maximalPalindromesIn' == nrOfCenters - centerfactor -}
testMoveCenterSGuard3 =
    TestCase $
        assertEqual
            "testMoveCenterSGuard3"
            [0, 1, 0, 1, 2, 1, 0, 1, 0, 1, 0, 11, 0, 1, 0, 1, 0, 1, 2, 1, 0, 1, 0]
            ( P.moveCenterS
                False
                (V.fromList "meetsysteem")
                11
                [11, 0, 1, 0, 1, 0, 1, 2, 1, 0, 1, 0]
                [0, 1, 0, 1, 0, 1, 2, 1, 0, 1, 0]
                11
            )

{- Tests moveCenterS on a DNA string -}
testMoveCenterSDNA =
    TestCase $
        assertEqual
            "testMoveCenterSDNA"
            [0, 2, 0, 0, 0, 6, 0, 0, 0, 2, 0]
            ( P.moveCenterS
                True
                (V.fromList [A, T, G, A, C, G, T, C, C, G])
                8
                [6, 0, 0, 0, 2, 0]
                [0, 0, 0, 2, 0]
                6
            )

{- Tests moveCenterS on a DNA string, not skipping any centers -}
testMoveCenterSDNAAllCenters =
    TestCase $
        assertEqual
            "testMoveCenterSDNAAllCenters"
            [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0]
            ( P.moveCenterS
                False
                (V.fromList [A, T, G, A, C, G, T, C, C, G])
                8
                [6, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0]
                [0, 0, 0, 0, 0, 0, 0, 2, 0, 0]
                6
            )

{-
----------------------------------------------------------
    End tests for moveCenterS
----------------------------------------------------------
-}

{-
----------------------------------------------------------
    Begin tests for finalPalindromeS
----------------------------------------------------------
-}

{- Test a simple case for finalPalindromesS, where no palindrome needs to be truncated.
This test is based on input string "leepeel" -}
testFinalPalindromesSSimple =
    TestCase $
        assertEqual
            "testFinalPalindromesSSimple"
            [0, 1, 0, 1, 2, 1, 0, 7, 0, 1, 2, 1, 0, 1, 0]
            ( P.finalPalindromesS
                False
                7
                [0, 1, 2, 1, 0, 1, 0]
                [7, 0, 1, 2, 1, 0, 1, 0]
            )

{- Test a case where some palindromes need to be truncated. This test is based on input
string "aaaaaaaaabaaaaaa", so 9x 'a', 1x 'b' and 6x 'a'. -}
testFinalPalindromesSCutOff =
    TestCase $
        assertEqual
            "testFinalPalindromesSCutOff"
            [ 0
            , 1
            , 2
            , 3
            , 4
            , 5
            , 6
            , 5
            , 4
            , 3
            , 2
            , 1
            , 0
            , 13
            , 0
            , 1
            , 2
            , 3
            , 4
            , 5
            , 6
            , 7
            , 8
            , 9
            , 8
            , 7
            , 6
            , 5
            , 4
            , 3
            , 2
            , 1
            , 0
            ]
            ( P.finalPalindromesS
                False
                13
                [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
                [13, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
            )

{- Test a case where the palindrome initiating finalPalindromesS does not contain another
palindrome. This test is based on input string "abcd". -}
testFinalPalindromesNrOfCentersZero =
    TestCase $
        assertEqual
            "testFinalPalindromesNrOfCentersZero"
            [0, 1, 0, 1, 0, 1, 0, 1, 0]
            ( P.finalPalindromesS
                False
                1
                [0, 1, 0, 1, 0, 1, 0]
                [1, 0, 1, 0, 1, 0, 1, 0]
            )

{- Test a DNA case with a palindrome in the palindrome which initiates finalPalindromeS.
This test is based on input string "TGCATG" -}
testFinalPalindromesSDNA =
    TestCase $
        assertEqual
            "testFinalPalindromesSDNA"
            [0, 0, 4, 0, 4, 0, 0]
            ( P.finalPalindromesS
                True
                4
                [0, 4, 0, 0]
                [4, 0, 4, 0, 0]
            )

{- Test a DNA case, where some palindrome needs to be truncated to a non-zero value.
This test is based on input string "TGCATGC" -}
testFinalPalindromesSDNACutOff =
    TestCase $
        assertEqual
            "testFinalPalindromesSDNACutOff"
            [0, 2, 0, 6, 0, 4, 0, 0]
            ( P.finalPalindromesS
                True
                6
                [0, 4, 0, 0]
                [6, 0, 4, 0, 0]
            )

{-
----------------------------------------------------------
    End tests for finalPalindromeS
----------------------------------------------------------
-}
