{-# LANGUAGE MonoLocalBinds #-}

{- |
Module      :  Data.Algorithms.Palindromes.Internal.QuadraticAlgorithm
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

This module contains an implementation of a quadratic algorithm to find palindromes.
-}
module Data.Algorithms.Palindromes.Internal.QuadraticAlgorithm
    ( maxPalindromePerCenter
    , getLeftRightCenterBetweenElems
    , getLeftRightCenterOnElem
    , lengthPalAtCenter
    , lengthPalAtCenterOnlyEvenPals
    , getLeftRight
    , lengthApproximatePalindrome
    ) where

import Data.List as L

import Data.Algorithms.Palindromes.PalEq
    ( PalEq (..)
    )

import qualified Data.Vector.Generic as G

{- | For each center, finds the maximal palindrome around this center.
Taking gaps and substition errors into account. This function runs in O(m),
where m is the sum of palindrome sizes.
-}
maxPalindromePerCenter
    :: (PalEq a, G.Vector v a)
    => Bool
    -> Int
    -> Int
    -> v a
    -> [Int]
maxPalindromePerCenter onlyEvenPals gapSize maxErrors input
    | onlyEvenPals =
        L.map
            (lengthPalAtCenterOnlyEvenPals input gapSize maxErrors)
            (if even gapSize then [0 .. G.length input] else [0 .. G.length input - 1])
    | otherwise =
        L.map
            (lengthPalAtCenter input gapSize maxErrors)
            [0 .. 2 * G.length input]

{- | Keep expanding the palindrome around the given center to get the maximal palindrome.
Allows a maximum of maxErrors errors. This function runs in O(k), where k is the size of
the found palindrome.
-}
lengthApproximatePalindrome
    :: (PalEq a, G.Vector v a) => v a -> Int -> Int -> Int -> Int
lengthApproximatePalindrome input maxErrors start end
    | start < 0 || end > lastPos = end - start - 1
    | (input G.! start) =:= (input G.! end) =
        lengthApproximatePalindrome input maxErrors (start - 1) (end + 1)
    | maxErrors > 0 =
        lengthApproximatePalindrome input (maxErrors - 1) (start - 1) (end + 1)
    | otherwise = end - start - 1
  where
    lastPos :: Int
    lastPos = G.length input - 1

{-
---------------------------------------------------------------------
      Begin getLeftRight functions
---------------------------------------------------------------------
-}

{- | Get the element index for the left and right characters to start expanding the
palindrome from, essentially ignoring the gap. This function must be used when the
palindrome center is between two elements. Note that if the inputted gap size is odd, the
actually used gap size will be (gapSize - 1).
-}
getLeftRightCenterBetweenElems
    :: Int
    -> Int
    -- ^ The index of the element to the right of the center.
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
    -- make sure halfg' is not larger than leewayLeft or leewayRight.
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
    -- ^ The index of the element on the center.
    -> Int
    -> (Int, Int)
getLeftRightCenterOnElem gapSize elementIndex lengthInput = (left, right)
  where
    halfg = (gapSize + 1) `div` 2
    {- How far the gap can span to the left without going out of
    bounds to the left. -}
    leewayLeft = elementIndex + 1
    {- How far the gap can span to the right without going out of.
    bounds to the right. -}
    leewayRight = lengthInput - elementIndex
    -- make sure halfg' is not larger than leewayLeft or leewayRight.
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
      Begin quadratic functions
---------------------------------------------------------------------
-}

-- | The length of the maximal palindrome around the specified center
lengthPalAtCenter
    :: (PalEq a, G.Vector v a)
    => v a
    -- ^ The total vector to find palindromes in.
    -> Int
    -- ^ The size of the gap.
    -> Int
    -- ^ The (maximum) number of allowed substitution errors.
    -> Int
    -- ^ The index of the center if searching for odd and even palindromes.
    -> Int
    -- ^ The found length of the maximal palindrome.
lengthPalAtCenter input gapSize maxErrors center =
    let (left, right) = getLeftRight gapSize center (G.length input)
    in  lengthApproximatePalindrome input maxErrors left right

{- | Get the two new character indexes for the left and right character to
start expanding from, ignoring the gap.
-}
getLeftRight
    :: Int
    -- ^ gap size.
    -> Int
    -- ^ center index if searching for odd and even palindromes.
    -> Int
    -- ^ the size of the whole input.
    -> (Int, Int)
getLeftRight gapSize center lengthInput
    | even center = getLeftRightCenterBetweenElems gapSize elementIndex lengthInput
    | otherwise = getLeftRightCenterOnElem gapSize elementIndex lengthInput
  where
    elementIndex = center `div` 2

{-
---------------------------------------------------------------------
      End quadratic functions
---------------------------------------------------------------------
-}

{-
---------------------------------------------------------------------
      Begin 'only even pals' quadratic functions
---------------------------------------------------------------------
-}

{- | The length of the maximal palindrome around the specified center for when we only
have to check for even palindromes. This is a separate function because the center indices
then are different than when we also have to check for odd palindromes, because there are
fewer centers to check now.
-}
lengthPalAtCenterOnlyEvenPals
    :: (PalEq a, G.Vector v a)
    => v a
    -- ^ The total vector to find palindromes in.
    -> Int
    -- ^ The size of the gap.
    -> Int
    -- ^ The (maximum) number of allowed substitution errors.
    -> Int
    -- ^ The index of the center.
    -> Int
    -- ^ The found length of the maximal palindrome.
lengthPalAtCenterOnlyEvenPals input gapSize maxErrors center =
    {- We can just use getLeftRightCenterBetweenElems, because we know the center is always
    between two elements. -}
    let (left, right) = getLeftRightCenterBetweenElems gapSize center (G.length input)
    in  lengthApproximatePalindrome input maxErrors left right

{-
---------------------------------------------------------------------
      End 'only even pals' quadratic functions
---------------------------------------------------------------------
-}
