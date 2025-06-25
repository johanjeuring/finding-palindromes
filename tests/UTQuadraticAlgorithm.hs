{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring -}

module UTQuadraticAlgorithm (testListQuadraticAlgorithm) where

import Test.HUnit (Test (..), assertEqual)

import Data.Algorithms.Palindromes.DNA
    ( DNA (..)
    )

import qualified Data.Vector.Unboxed as U

import qualified Data.Algorithms.Palindromes.Internal.QuadraticAlgorithm as Q

testListQuadraticAlgorithm =
    [ testMaxPalindromePerCenterDNA
    , testMaxPalindromePerCenterText
    , testLengthPalAtCenterEven
    , testLengthGappedPalAtCenterEven
    , testLengthPalWithErrorsAtCenterEven
    , testLengthGappedPalWithErrorsAtCenterEven
    , testLengthPalAtCenterOdd
    , testLengthGappedPalAtCenterOdd
    , testLengthPalWithErrorsAtCenterOdd
    , testLengthGappedPalWithErrorsAtCenterOdd
    , testGetLeftRightEven
    , testGetLeftRightOdd
    , testLengthPalAtCenterOnlyEvenPals
    , testLengthGappedPalAtCenterOnlyEvenPals
    , testLengthPalWithErrorsAtCenterOnlyEvenPals
    , testLengthGappedPalWithErrorsAtCenterOnlyEvenPals
    , testLeftOutOfBoundsLengthApproximatePalindrome
    , testRightOutOfBoundsLengthApproximatePalindrome
    , testNoErrorLengthApproximatePalindrome
    , testErrorLengthApproximatePalindrome
    ]

{-
----------------------------------------------------------
    Begin tests for maxPalindromePerCenter
----------------------------------------------------------
-}

-- | Test maxPalindromePerCenter on some small DNA input
testMaxPalindromePerCenterDNA =
    TestCase $
        assertEqual
            "testMaxPalindromePerCenterDNA"
            [0, 2, 0, 2, 0]
            ( Q.maxPalindromePerCenter
                True
                0
                0
                (U.fromList [A, T, C, G])
            )

-- | Test maxPalindromePerCenter on some small text input
testMaxPalindromePerCenterText =
    TestCase $
        assertEqual
            "testMaxPalindromePerCenterText"
            [0, 1, 0, 1, 4, 1, 0, 3, 0, 1, 0]
            ( Q.maxPalindromePerCenter
                False
                0
                0
                (U.fromList "abbab")
            )

{-
----------------------------------------------------------
    End tests for maxPalindromePerCenter
----------------------------------------------------------
-}

{-
-------------------------------------------------
    Begin tests for lengthPalAtCenter
-------------------------------------------------
-}

-- | Test lengthPalAtCenter with even center index, no gap and no errors
testLengthPalAtCenterEven =
    TestCase $
        assertEqual
            "testlengthPalAtCenterEven"
            2
            (Q.lengthPalAtCenter (U.fromList "yabadabadoo") 0 0 20)

-- | Test lengthPalAtCenter with even center index, a gap and no errors
testLengthGappedPalAtCenterEven =
    TestCase $
        assertEqual
            "testLengthGappedPalAtCenterEven"
            6
            (Q.lengthPalAtCenter (U.fromList "yabaddabadoo") 4 0 14)

-- | Test lengthPalAtCenter with even center index, no gap and with errors
testLengthPalWithErrorsAtCenterEven =
    TestCase $
        assertEqual
            "testLengthPalWithErrorsAtCenterEven"
            6
            (Q.lengthPalAtCenter (U.fromList "yabaoabadoo") 0 2 14)

-- | Test lengthPalAtCenter with even center index, a gap and with errors
testLengthGappedPalWithErrorsAtCenterEven =
    TestCase $
        assertEqual
            "testLengthGappedPalWithErrorsAtCenterEven"
            10
            (Q.lengthPalAtCenter (U.fromList "yabaddabadoo") 4 3 14)

-- | Test lengthPalAtCenter with odd center index, no gap and no errors
testLengthPalAtCenterOdd =
    TestCase $
        assertEqual
            "testlengthPalAtCenterEven"
            7
            (Q.lengthPalAtCenter (U.fromList "yabadabadoo") 0 0 9)

-- | Test lengthPalAtCenter with odd center index, a gap and no errors
testLengthGappedPalAtCenterOdd =
    TestCase $
        assertEqual
            "testLengthGappedPalAtCenterOdd"
            7
            (Q.lengthPalAtCenter (U.fromList "yabbagapabadoo") 3 0 13)

-- | Test lengthPalAtCenter with odd center index, no gap and with errors
testLengthPalWithErrorsAtCenterOdd =
    TestCase $
        assertEqual
            "testLengthPalWithErrorsAtCenterOdd"
            9
            (Q.lengthPalAtCenter (U.fromList "yabadabadoo") 0 2 13)

-- | Test lengthPalAtCenter with odd center index, a gap and with errors
testLengthGappedPalWithErrorsAtCenterOdd =
    TestCase $
        assertEqual
            "testLengthGappedPalWithErrorsAtCenterOdd"
            11
            (Q.lengthPalAtCenter (U.fromList "yabbagapabadoo") 3 2 13)

{-
-------------------------------------------------
    End tests for lengthPalAtCenter
-------------------------------------------------
-}

{-
---------------------------------------------
    Begin tests for getLeftRight
---------------------------------------------
-}

-- | Test getLeftRight with an even center index
testGetLeftRightEven =
    TestCase $
        assertEqual
            "testgetLeftRightEven"
            (0, 3)
            (Q.getLeftRight 2 4 7)

-- | Test getLeftRight with an odd center index
testGetLeftRightOdd =
    TestCase $
        assertEqual
            "testgetLeftRightOdd"
            (1, 5)
            (Q.getLeftRight 3 7 7)

{-
---------------------------------------------
    End tests for getLeftRight
---------------------------------------------
-}

{-
--------------------------------------------------------
    Begin tests for 'only even pals' length pal at center
--------------------------------------------------------
-}

{- | Test whether the observed length of the palindrome with (anti-reflexive) DNA
is correct for palindrome without gaps and without errors.
-}
testLengthPalAtCenterOnlyEvenPals =
    TestCase $
        assertEqual
            "testLengthPalAtCenterOnlyEvenPals"
            4
            ( Q.lengthPalAtCenterOnlyEvenPals
                (U.fromList [A, A, T {-center-}, A, T, G] :: U.Vector DNA)
                0
                0
                3
            )

{- | Test whether the observed length of the palindrome with (anti-reflexive) DNA
is correct for palindrome with an even gap and without errors.
-}
testLengthGappedPalAtCenterOnlyEvenPals =
    TestCase $
        assertEqual
            "testLengthGappedPalAtCenterOnlyEvenPals"
            8
            ( Q.lengthPalAtCenterOnlyEvenPals
                --                  gap: |-------------------|
                (U.fromList [A, A, T, G, T, G {-center-}, A, A, C, A, A, T, C] :: U.Vector DNA)
                4
                0
                6
            )

{- | Test whether the observed length of the palindrome with (anti-reflexive) DNA
is correct for palindrome without no gap and with errors.
-}
testLengthPalWithErrorsAtCenterOnlyEvenPals =
    TestCase $
        assertEqual
            "testLengthPalWithErrorsAtCenterOnlyEvenPals"
            10
            ( Q.lengthPalAtCenterOnlyEvenPals
                (U.fromList [A, A, A, T, G, T, G {-center-}, A, A, C, A, A, G, C] :: U.Vector DNA)
                0
                2
                7
            )

{- | Test whether the observed length of the palindrome with (anti reflexive) DNA
is correct for palindrome with even gap and with errors.
-}
testLengthGappedPalWithErrorsAtCenterOnlyEvenPals =
    TestCase $
        assertEqual
            "testLengthGappedPalWithErrorsAtCenterOnlyEvenPals"
            8
            ( Q.lengthPalAtCenterOnlyEvenPals
                (U.fromList [A, A, A, T {-center-}, G, T, G, A, A, C, A, A, G, C] :: U.Vector DNA)
                2
                2
                4
            )

{-
--------------------------------------------------------
    Begin tests for length approximate palindrome
--------------------------------------------------------
-}

-- | Tests the case where the a negative index is given to the function
testLeftOutOfBoundsLengthApproximatePalindrome =
    TestCase $
        assertEqual
            "testLeftOutOfBoundsLengthApproximatePalindrome"
            2
            ( Q.lengthApproximatePalindrome
                (U.fromList "yay")
                0
                (-1)
                2
            )

-- | Tests the case where the a index is given which is larger then the vector size
testRightOutOfBoundsLengthApproximatePalindrome =
    TestCase $
        assertEqual
            "testRightOutOfBoundsLengthApproximatePalindrome"
            2
            ( Q.lengthApproximatePalindrome
                (U.fromList "yay")
                0
                0
                3
            )

-- | Tests the case where a palindrome is given without allowing any errors
testNoErrorLengthApproximatePalindrome =
    TestCase $
        assertEqual
            "testNoErrorLengthApproximatePalindrome"
            9
            ( Q.lengthApproximatePalindrome
                (U.fromList "hallollah")
                0
                4
                4
            )

-- | Tests the case where a palindrome is given with errors
testErrorLengthApproximatePalindrome =
    TestCase $
        assertEqual
            "testErrorLengthApproximatePalindrome"
            9
            ( Q.lengthApproximatePalindrome
                (U.fromList "hellollah")
                1
                4
                4
            )

{-
------------------------------------------------------
    End tests for length approximate palindrome
--------------------------------------------------------
-}
