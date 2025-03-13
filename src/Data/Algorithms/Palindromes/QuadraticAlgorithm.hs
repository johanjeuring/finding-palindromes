module Data.Algorithms.Palindromes.QuadraticAlgorithm
    ( gappedApproximatePalindromesAroundCentres
    , getLeftRightCenterBetweenElems
    , getLeftRightCenterOnElem
    , lengthPalAtCenterReflexive
    , lengthPalAtCenterAntiReflexive
    , getLeftRightReflexive
    , lengthApproximatePalindrome
    ) where

import Data.Algorithms.Palindromes.PalindromesUtils
    ( Couplable (..)
    , Flag (DNA)
    )
import Data.List as L
import Data.Vector as V

{- | For each center, finds the maximal palindrome around this center.
This function runs in O(m), where m is the sum of palindrome sizes.
-}
gappedApproximatePalindromesAroundCentres
    :: (Couplable a)
    => Maybe Flag
    -> V.Vector a
    -> Int
    -> Int
    -> [Int]
gappedApproximatePalindromesAroundCentres palindromeVariant input gapSize errorCount =
    case palindromeVariant of
        Just DNA ->
            L.map
                (lengthPalAtCenterAntiReflexive input gapSize errorCount)
                (if even gapSize then [0 .. V.length input] else [0 .. V.length input - 1])
        _ ->
            L.map
                (lengthPalAtCenterReflexive input gapSize errorCount)
                [0 .. 2 * V.length input]

{- | Keep expanding the palindrome around the given center to get the maximal palindrome.
Allows a maximum of errorCount errors. This function runs in O(k), where k is the size of the
found palindrome.
-}
lengthApproximatePalindrome
    :: (Couplable a) => V.Vector a -> Int -> Int -> Int -> Int
lengthApproximatePalindrome input errorCount start end
    | start < 0 || end > lastPos = end - start - 1
    | (input V.! start) =:= (input V.! end) =
        lengthApproximatePalindrome input errorCount (start - 1) (end + 1)
    | errorCount > 0 =
        lengthApproximatePalindrome input (errorCount - 1) (start - 1) (end + 1)
    | otherwise = end - start - 1
  where
    lastPos :: Int
    lastPos = V.length input - 1

{-
---------------------------------------------------------------------
      Begin getLeftRight functions
---------------------------------------------------------------------
-}

{- | Get the element index for the left and right characters to start expanding the
palindrome from, essentially ignoring the gap. This function must be used when the
palindrome center is between two elements. Note that if the gapSize is odd, the
resulting gap will be maximum (gapSize - 1) big.
-}
getLeftRightCenterBetweenElems
    :: Int
    -> Int
    -- ^ The index of the element to the right of the center
    -> Int
    -> (Int, Int)
getLeftRightCenterBetweenElems gapSize elementIndex lengthInput = (left, right)
  where
    halfg = gapSize `div` 2
    {- How far the gap can span to the left without going out of
    bounds to the left. -}
    leewayLeft = elementIndex
    {- How far the gap can span to the right without going out of
    bounds to the right. -}
    leewayRight = lengthInput - elementIndex
    -- make sure halfg' is not larger than leewayLeft or leewayRight
    halfg' = L.minimum [halfg, leewayLeft, leewayRight]
    left = elementIndex - 1 - halfg'
    right = elementIndex + halfg'

{- | Get the element index for the left and right characters to start expanding the
palindrome from, essentially ignoring the gap. This function must be used when the
palindrome center is on an element. Note that if the gapSize is even, the
resulting gap will be maximum (gapSize - 1) big.
-}
getLeftRightCenterOnElem
    :: Int
    -> Int
    -- ^ The index of the element on the center
    -> Int
    -> (Int, Int)
getLeftRightCenterOnElem gapSize elementIndex lengthInput = (left, right)
  where
    halfg = (gapSize + 1) `div` 2
    {- How far the gap can span to the left without going out of
    bounds to the left. -}
    leewayLeft = elementIndex + 1
    {- How far the gap can span to the right without going out of
    bounds to the right. -}
    leewayRight = lengthInput - elementIndex
    -- make sure halfg' is not larger than leewayLeft or leewayRight
    halfg' = L.minimum [halfg, leewayLeft, leewayRight]
    left = elementIndex - halfg'
    right = elementIndex + halfg'

{-
---------------------------------------------------------------------
      End getLeftRight functions
---------------------------------------------------------------------
-}

{-
---------------------------------------------------------------------
      Begin reflexive quadratic functions
---------------------------------------------------------------------
-}

-- | The length of the maximal palindrome around the specified center
lengthPalAtCenterReflexive
    :: (Couplable a)
    => V.Vector a
    -- ^ The total vector to find palindromes in
    -> Int
    -- ^ The (maximum) size of the gap
    -> Int
    -- ^ The (maximum) number of allowed errors
    -> Int
    -- ^ The index of the center
    -> Int
    -- ^ The resulting length of the found maximal palindrome
lengthPalAtCenterReflexive input gapSize errorCount center =
    let (left, right) = getLeftRightReflexive gapSize center (V.length input)
    in  lengthApproximatePalindrome input errorCount left right

{- | Get the two new character indexes for the left and right character to
start expanding from, ignoring the gap.
-}
getLeftRightReflexive
    :: Int
    -- ^ gap size
    -> Int
    -- ^ center index
    -> Int
    -- ^ the size of the whole input
    -> (Int, Int)
getLeftRightReflexive gapSize center lengthInput
    | even center = getLeftRightCenterBetweenElems gapSize element lengthInput
    | otherwise = getLeftRightCenterOnElem gapSize element lengthInput
  where
    element = center `div` 2

{-
---------------------------------------------------------------------
      End reflexive quadratic functions
---------------------------------------------------------------------
-}

{-
---------------------------------------------------------------------
      Begin anti-reflexive quadratic functions
---------------------------------------------------------------------
-}

lengthPalAtCenterAntiReflexive
    :: (Couplable a)
    => V.Vector a
    -- ^ The total vector to find palindromes in
    -> Int
    -- ^ The (maximum) size of the gap
    -> Int
    -- ^ The (maximum) number of allowed errors
    -> Int
    -- ^ The index of the center
    -> Int
lengthPalAtCenterAntiReflexive input gapSize errorCount center =
    -- We can just use getLeftRightCenterBetweenElems, because with anti reflexive data
    -- types, the center is aways between two elements
    let (left, right) = getLeftRightCenterBetweenElems gapSize center (V.length input)
    in  lengthApproximatePalindrome input errorCount left right

{-
---------------------------------------------------------------------
      End anti-reflexive quadratic functions
---------------------------------------------------------------------
-}
