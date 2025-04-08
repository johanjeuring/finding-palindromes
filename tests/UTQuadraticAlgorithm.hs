{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}

module UTQuadraticAlgorithm (testListQuadraticAlgorithm) where

import Data.Algorithms.Palindromes.DNA
    ( DNA (..)
    )
import Test.HUnit (Test (..), assertEqual)

import qualified Data.Algorithms.Palindromes.QuadraticAlgorithm as Q
import qualified Data.Vector as V

testListQuadraticAlgorithm =
    [ testGappedApproximatePalindromesAroundCentresDNA
    , testGappedApproximatePalindromesAroundCentresText
    , testLengthPalAtCenterReflexiveEven
    , testLengthGappedPalAtCenterReflexiveEven
    , testLengthPalWithErrorsAtCenterReflexiveEven
    , testLengthGappedPalWithErrorsAtCenterReflexiveEven
    , testLengthPalAtCenterReflexiveOdd
    , testLengthGappedPalAtCenterReflexiveOdd
    , testLengthPalWithErrorsAtCenterReflexiveOdd
    , testLengthGappedPalWithErrorsAtCenterReflexiveOdd
    , testGetLeftRightReflexiveEven
    , testGetLeftRightReflexiveOdd
    , testLengthPalAtCenterAntiReflexive
    , testLengthGappedPalAtCenterAntiReflexive
    , testLengthPalWithErrorsAtCenterAntiReflexive
    , testLengthGappedPalWithErrorsAtCenterAntiReflexive
    , testLeftOutOfBoundsLengthApproximatePalindrome
    , testRightOutOfBoundsLengthApproximatePalindrome
    , testNoErrorLengthApproximatePalindrome
    , testErrorLengthApproximatePalindrome
    ]

{-
----------------------------------------------------------
    Begin tests for gappedApproximatePalindromesAroundCentres
----------------------------------------------------------
-}

-- | Test gappedApproximatePalindromesAroundCentres on some small DNA input
testGappedApproximatePalindromesAroundCentresDNA =
    TestCase $
        assertEqual
            "testGappedApproximatePalindromesAroundCentresDNA"
            [0, 2, 0, 2, 0]
            ( Q.gappedApproximatePalindromesAroundCentres
                True
                0
                0
                (V.fromList [A, T, C, G])
            )

-- | Test gappedApproximatePalindromesAroundCentres on some small text input
testGappedApproximatePalindromesAroundCentresText =
    TestCase $
        assertEqual
            "testGappedApproximatePalindromesAroundCentresText"
            [0, 1, 0, 1, 4, 1, 0, 3, 0, 1, 0]
            ( Q.gappedApproximatePalindromesAroundCentres
                False
                0
                0
                (V.fromList "abbab")
            )

{-
----------------------------------------------------------
    End tests for gappedApproximatePalindromesAroundCentres
----------------------------------------------------------
-}

{-
-------------------------------------------------
    Begin tests for lengthPalAtCenterReflexive
-------------------------------------------------
-}

-- | Test lengthPalAtCenterReflexive with even center index, no gap and no errors
testLengthPalAtCenterReflexiveEven =
    TestCase $
        assertEqual
            "testLengthPalAtCenterReflexiveEven"
            2
            (Q.lengthPalAtCenterReflexive (V.fromList "yabadabadoo") 0 0 20)

-- | Test lengthPalAtCenterReflexive with even center index, a gap and no errors
testLengthGappedPalAtCenterReflexiveEven =
    TestCase $
        assertEqual
            "testLengthGappedPalAtCenterReflexiveEven"
            6
            --                                       gap: |--|
            (Q.lengthPalAtCenterReflexive (V.fromList "yabaddabadoo") 4 0 14)

-- | Test lengthPalAtCenterReflexive with even center index, no gap and with errors
testLengthPalWithErrorsAtCenterReflexiveEven =
    TestCase $
        assertEqual
            "testLengthGappedPalAtCenterReflexiveEven"
            6
            (Q.lengthPalAtCenterReflexive (V.fromList "yabaoabadoo") 0 2 14)

-- | Test lengthPalAtCenterReflexive with even center index, a gap and with errors
testLengthGappedPalWithErrorsAtCenterReflexiveEven =
    TestCase $
        assertEqual
            "testLengthGappedPalWithErrorsAtCenterReflexiveEven"
            10
            --                                       gap: |--|
            (Q.lengthPalAtCenterReflexive (V.fromList "yabaddabadoo") 4 3 14)

-- | Test lengthPalAtCenterReflexive with odd center index, no gap and no errors
testLengthPalAtCenterReflexiveOdd =
    TestCase $
        assertEqual
            "testLengthPalAtCenterReflexiveEven"
            7
            (Q.lengthPalAtCenterReflexive (V.fromList "yabadabadoo") 0 0 9)

-- | Test lengthPalAtCenterReflexive with odd center index, a gap and no errors
testLengthGappedPalAtCenterReflexiveOdd =
    TestCase $
        assertEqual
            "testLengthGappedPalAtCenterReflexiveOdd"
            7
            --                                       gap: |-|
            (Q.lengthPalAtCenterReflexive (V.fromList "yabbagapabadoo") 3 0 13)

-- | Test lengthPalAtCenterReflexive with odd center index, no gap and with errors
testLengthPalWithErrorsAtCenterReflexiveOdd =
    TestCase $
        assertEqual
            "testLengthPalWithErrorsAtCenterReflexiveOdd"
            9
            (Q.lengthPalAtCenterReflexive (V.fromList "yabadabadoo") 0 2 13)

-- | Test lengthPalAtCenterReflexive with odd center index, a gap and with errors
testLengthGappedPalWithErrorsAtCenterReflexiveOdd =
    TestCase $
        assertEqual
            "testLengthGappedPalWithErrorsAtCenterReflexiveOdd"
            11
            --                                       gap: |-|
            (Q.lengthPalAtCenterReflexive (V.fromList "yabbagapabadoo") 3 2 13)

{-
-------------------------------------------------
    End tests for lengthPalAtCenterReflexive
-------------------------------------------------
-}

{-
---------------------------------------------
    Begin tests for getLeftRightReflexive reflexive
---------------------------------------------
-}

-- | Test getLeftRightReflexive with an even center index
testGetLeftRightReflexiveEven =
    TestCase $
        assertEqual
            "testGetLeftRightReflexiveEven"
            (0, 3)
            (Q.getLeftRightReflexive 2 4 7)

-- | Test getLeftRightReflexive with an odd center index
testGetLeftRightReflexiveOdd =
    TestCase $
        assertEqual
            "testGetLeftRightReflexiveOdd"
            (1, 5)
            (Q.getLeftRightReflexive 3 7 7)

{-
---------------------------------------------
    End tests for getLeftRightReflexive reflexive
---------------------------------------------
-}

{-
--------------------------------------------------------
    Begin tests for anti-reflexive length pal at center
--------------------------------------------------------
-}

{- | Test whether the observed length of the palindrome with (anti reflexive) DNA
is correct for palindrome without gaps and without errors.
-}
testLengthPalAtCenterAntiReflexive =
    TestCase $
        assertEqual
            "testLengthPalAtCenterAntiReflexive"
            4
            ( Q.lengthPalAtCenterAntiReflexive
                (V.fromList [A, A, T {-center-}, A, T, G] :: V.Vector DNA)
                0
                0
                3
            )

{- | Test whether the observed length of the palindrome with (anti reflexive) DNA
is correct for palindrome with a gap and without errors.
-}
testLengthGappedPalAtCenterAntiReflexive =
    TestCase $
        assertEqual
            "testLengthGappedPalAtCenterAntiReflexive"
            8
            ( Q.lengthPalAtCenterAntiReflexive
                --                  gap: |-------------------|
                (V.fromList [A, A, T, G, T, G {-center-}, A, A, C, A, A, T, C] :: V.Vector DNA)
                4
                0
                6
            )

{- | Test whether the observed length of the palindrome with (anti reflexive) DNA
is correct for palindrome without a gap and with errors.
-}
testLengthPalWithErrorsAtCenterAntiReflexive =
    TestCase $
        assertEqual
            "testLengthPalWithErrorsAtCenterAntiReflexive"
            10
            ( Q.lengthPalAtCenterAntiReflexive
                (V.fromList [A, A, A, T, G, T, G {-center-}, A, A, C, A, A, G, C] :: V.Vector DNA)
                0
                2
                7
            )

{- | Test whether the observed length of the palindrome with (anti reflexive) DNA
is correct for palindrome with gap and with errors.
-}
testLengthGappedPalWithErrorsAtCenterAntiReflexive =
    TestCase $
        assertEqual
            "testLengthGappedPalWithErrorsAtCenterAntiReflexive"
            8
            ( Q.lengthPalAtCenterAntiReflexive
                (V.fromList [A, A, A, T {-center-}, G, T, G, A, A, C, A, A, G, C] :: V.Vector DNA)
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
                (V.fromList "yay")
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
                (V.fromList "yay")
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
                (V.fromList "hallollah")
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
                (V.fromList "hellollah")
                1
                4
                4
            )

{-
------------------------------------------------------
    End tests for length approximate palindrome
--------------------------------------------------------
-}
