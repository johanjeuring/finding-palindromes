module UTLinearAlgorithm (testListLinearAlgorithm) where

import Data.Algorithms.Palindromes.LinearAlgorithm as P
import Test.HUnit (Test (..), assertEqual)

testListLinearAlgorithm =
    [ testFinalPalindromesSSimple
    , testFinalPalindromesSCutOff
    , testFinalPalindromesNrOfCentersZero
    , testFinalPalindromesSDNA
    ]

-- This test is based on input string "leepeel"
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

-- This test is based on input string "aaaaaaaaabaaaaaa", so 9x 'a', 1x 'b' and 6x 'a'.
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

-- This test is based on input string "abcd"
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

-- This test is based on input string "TGCATG"
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
