module UTLinearAlgorithm (testListLinearAlgorithm) where

import Test.HUnit (Test (..), assertEqual)
import qualified Data.Algorithms.Palindromes.LinearAlgorithm as P
import qualified Data.Vector as V
import Control.Exception (assert)

import Data.Algorithms.Palindromes.PalindromesUtils (toDNA)

testListLinearAlgorithm =
    [ testFinalPalindromesSSimple
    , testFinalPalindromesSCutOff
    , testFinalPalindromesNrOfCentersZero
    , testFinalPalindromesSDNA
    , testExtendPalindromeSSimple
    , testExtendPalindromeSWhole
    , testExtendPalindromeSIntertwined
    , testExtendPalindromeSNothing
    , testExtendPalindromeSEmpty
    , testExtendPalindromeSDNA
    ]

{-
----------------------------------------------------------
    Begin tests for finalPalindromeS
----------------------------------------------------------
-}

testExtendPalindromeS =
    TestCase $
        assertEqual
            "testFinalPalindromesSSimple"
            [0, 1, 0, 1, 2, 1, 0 {-, 7, 0, 1, 2, 1, 0, 1, 0-}]
            ( P.finalPalindromesS'
                1
                7
                -- [7, 0, 1, 2, 1, 0, 1, 0]
                [0, 1, 2, 1, 0, 1, 0]
            )

{- Test a simple case for finalPalindromesS, where no palindrome needs to be truncated.
This test is based on input string "leepeel" -}
testFinalPalindromesSSimple =
    TestCase $
        assertEqual
            "testFinalPalindromesSSimple"
            [0, 1, 0, 1, 2, 1, 0 {-, 7, 0, 1, 2, 1, 0, 1, 0-}]
            ( P.finalPalindromesS'
                1
                7
                -- [7, 0, 1, 2, 1, 0, 1, 0]
                [0, 1, 2, 1, 0, 1, 0]
            )

{- Test a case where some palindromes need to be truncated. This test is based on input
string "aaaaaaaaabaaaaaa", so 9x 'a', 1x 'b' and 6x 'a'. -}
testFinalPalindromesSCutOff =
    TestCase $
        assertEqual
            "testFinalPalindromesSCutOff"
            ( [ 0
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
              , 0 {-, 13, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
                  ++ [8, 7, 6, 5, 4, 3, 2, 1, 0-}
              ]
            )
            ( P.finalPalindromesS'
                1
                13
                -- [13, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
                [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
            )

{- Test a case where the palindrome initiating finalPalindromesS does not contain another
palindrome. This test is based on input string "abcd". -}
testFinalPalindromesNrOfCentersZero =
    TestCase $
        assertEqual
            "testFinalPalindromesNrOfCentersZero"
            []
            {-0, 1, 0, 1, 0, 1, 0, 1, 0-}
            ( P.finalPalindromesS'
                1
                0
                -- [0, 1, 0, 1, 0, 1, 0, 1, 0]
                [1, 0, 1, 0, 1, 0, 1, 0]
            )

{- Test a DNA case with a palindrome in the palindrome which initiates finalPalindromeS.
This test is based on input string "TGCATG" -}
testFinalPalindromesSDNA =
    TestCase $
        assertEqual
            "testFinalPalindromesNrOfCentersZero"
            [0, 0 {-, 4, 0, 4, 0, 0-}]
            ( P.finalPalindromesS'
                2
                4
                -- [4, 0, 4, 0, 0]
                [0, 4, 0, 0]
            )

{-
----------------------------------------------------------
    End tests for finalPalindromeS
----------------------------------------------------------
-}
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
                1
                1
                (V.fromList "nnaba")
                []
                0
                0
            )

{- Test a string on extendPalindromeS consisting of one big palindrome. The output list should be symmetrical -}
testExtendPalindromeSWhole =
    TestCase $
        assertEqual
            "testExtendPalindromeSWhole"
            [0, 1, 0, 1, 2, 1, 0, 1, 0, 1, 0, 11, 0, 1, 0, 1, 0, 1, 2, 1, 0, 1, 0]
            ( P.extendPalindromeS
                1
                1
                (V.fromList "meetsysteem")
                []
                0
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
                1
                1
                (V.fromList "leepeeleep")
                []
                0
                0
            )

{- Test a string on extendPalindromeS not containing any palindromes -}
testExtendPalindromeSNothing =
    TestCase $
        assertEqual
            "testExtendPalindromeSNothing"
            [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]
            ( P.extendPalindromeS
                1
                1
                (V.fromList "abcdefgh")
                []
                0
                0
            )

{- Test an empty string on extendPalindromeS -}
testExtendPalindromeSEmpty =
    TestCase $
        assertEqual
            "testExtendPalindromeSEmpty"
            [0]
            ( P.extendPalindromeS
                1
                1
                (V.fromList "")
                []
                0
                0
            )

{- Test a DNA string on extendPalindromeS -}
testExtendPalindromeSDNA =
    TestCase $
        assertEqual
            "testExtendPalindromeSDNA"
            [0, 2, 0, 6, 0, 2, 0]
            ( P.extendPalindromeS
                2
                0
                (toDNA (V.fromList "atgcat"))
                []
                0
                0
            )

{-
----------------------------------------------------------
    End tests for finalPalindromeS
----------------------------------------------------------
-}
