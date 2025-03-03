module UTGetLeftRight (testListGetLeftRight) where

import Data.Algorithms.Palindromes.QuadraticAlgorithm
    ( getLeftRightCenterBetweenElems
    , getLeftRightCenterOnElem
    )
import Test.HUnit (Test (..), assertEqual)

testListGetLeftRight =
    [ testCenterBetweenElems
    , testCenterBetweenElemsOddGap
    , testCenterBetweenElemsOutOfBoundsLeft
    , testCenterBetweenElemsOutOfBoundsRight
    , testCenterBetweenElemsZeroGap
    , testCenterBetweenElemsGapLargerThanInput
    , testCenterOnElem
    , testCenterOnElemEvenGap
    , testCenterOnElemOutOfBoundsLeft
    , testCenterOnElemOutOfBoundsRight1
    , testCenterOnElemOutOfBoundsRight2
    , testCenterOnElemZeroGap
    , testCenterOnElemGapLargerThanInput
    ]

{-
---------------------------------------------------
Tests for center between elements
---------------------------------------------------
-}

-- | Test normal input with even gap and center between two elements.
testCenterBetweenElems =
    TestCase $
        assertEqual
            "testCenterBetweenElems"
            (2, 5)
            (getLeftRightCenterBetweenElems 2 4 7)

{- | Test whether inputting an odd gap in getLeftRightCenterBetweenElems behaves as
     expected.
-}
testCenterBetweenElemsOddGap =
    TestCase $
        assertEqual
            "testCenterBetweenElemsOddGap"
            (2, 5)
            (getLeftRightCenterBetweenElems 3 4 7)

{- | Test input with even gap and center between two elements where gap
goes out of bounds to the left.
-}
testCenterBetweenElemsOutOfBoundsLeft =
    TestCase $
        assertEqual
            "testCenterBetweenElemsOutOfBoundsLeft"
            (-1, 2)
            (getLeftRightCenterBetweenElems 4 1 7)

{- | Test input with even gap and center between two elements where gap
goes out of bounds to the right.
-}
testCenterBetweenElemsOutOfBoundsRight =
    TestCase $
        assertEqual
            "testCenterBetweenElemsOutOfBoundsRight"
            (4, 7)
            (getLeftRightCenterBetweenElems 8 6 7)

-- | Test input with gapsize of zero and center between two elements.
testCenterBetweenElemsZeroGap =
    TestCase $
        assertEqual
            "testCenterBetweenElemsZeroGap"
            (3, 4)
            (getLeftRightCenterBetweenElems 0 4 7)

{- | Test whether the out of bounds indices are returned if the gap is
    larger than the input and the center is between two elements.
-}
testCenterBetweenElemsGapLargerThanInput =
    TestCase $
        assertEqual
            "testCenterBetweenElemsGapLargerThanInput"
            (-1, 7)
            (getLeftRightCenterOnElem 9 3 7)

{-
---------------------------------------------------
Tests for center on element
---------------------------------------------------
-}

-- | Test normal input with odd gap and center on element.
testCenterOnElem =
    TestCase $
        assertEqual
            "testCenterOnElem"
            (1, 5)
            (getLeftRightCenterOnElem 3 3 7)

-- | Test whether inputting an even gap in getLeftRightCenterOnElem behaves as expected.
testCenterOnElemEvenGap =
    TestCase $
        assertEqual
            "testCenterOnElemEvenGap"
            (1, 5)
            (getLeftRightCenterOnElem 4 3 7)

{- | Test input with odd gap and center on element where gap
goes out of bounds to the left.
-}
testCenterOnElemOutOfBoundsLeft =
    TestCase $
        assertEqual
            "testCenterOnElemOutOfBoundsLeft"
            (-1, 3)
            (getLeftRightCenterOnElem 5 1 7)

{- | Test input with odd gap and center on element where gap
goes out of bounds to the right.
-}
testCenterOnElemOutOfBoundsRight1 =
    TestCase $
        assertEqual
            "testCenterOnElemOutOfBoundsRight1"
            (5, 7)
            (getLeftRightCenterOnElem 5 6 7)

{- | Test input with odd gap and center on element where gap
goes out of bounds to the right. This tests the same thing as the previous test,
because there were some subtle errors in the code, so we want to be
extra sure this works.
-}
testCenterOnElemOutOfBoundsRight2 =
    TestCase $
        assertEqual
            "testCenterOnElemOutOfBoundsRight2"
            (3, 7)
            (getLeftRightCenterOnElem 9 5 7)

{- | Test input with gapsize of zero and center on element. The output must have left
and right equal to the element index so the not anti-reflexive function can check
whether the element on the index is couplable with itself.
-}
testCenterOnElemZeroGap =
    TestCase $
        assertEqual
            "testCenterOnElemZeroGap"
            (4, 4)
            (getLeftRightCenterOnElem 0 4 7)

{- | Test whether the out of bounds indices are returned if the gap is
    larger than the input and the center is on an element.
-}
testCenterOnElemGapLargerThanInput =
    TestCase $
        assertEqual
            "testCenterOnElemGapLargerThanInput"
            (-1, 7)
            (getLeftRightCenterOnElem 9 3 7)
