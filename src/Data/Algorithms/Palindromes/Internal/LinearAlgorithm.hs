{-# LANGUAGE MonoLocalBinds #-}

{- |
Module      :  Data.Algorithms.Palindromes.Internal.LinearAlgorithm
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

This module contains an implementation of a linear algorithm to find palindromes based on a paper by Johan Jeuring.
-}
module Data.Algorithms.Palindromes.Internal.LinearAlgorithm
    ( extendPalindromeS
    , finalPalindromesS
    , moveCenterS
    ) where

import Data.Algorithms.Palindromes.PalEq (PalEq (..), palEqToItselfAtIndex)

import qualified Data.Vector.Generic as G

-- | This function traverses input linearly, using an accumulator.
extendPalindromeS
    :: (PalEq a, G.Vector v a)
    => Bool
    -- ^ Indicates whether the input datatype is anti-reflexive.
    -> v a
    -- ^ Input, with only the elements we want to find palindromes in.
    -> Int
    -- ^ The rightmost index which is checked by the algorithm.
    -> [Int]
    -- ^ Length of palindromes that are already found.
    -> Int
    -- ^ The length of the palindrome currently being expanded.
    -> [Int]
    -- ^ The final list of maximal palindrome lengths at every center position.
extendPalindromeS onlyEvenPals input rightmost maximalPalindromesIn currentPalindrome
    | rightmost > lastPos =
        -- reached the end of the array
        finalPalindromesS
            onlyEvenPals
            currentPalindrome
            maximalPalindromesIn
            (currentPalindrome : maximalPalindromesIn)
    | rightmost - currentPalindrome == first
        || not ((input G.! rightmost) =:= (input G.! (rightmost - currentPalindrome - 1))) =
        -- the current palindrome extends to the start of the array, or it cannot be
        -- extended
        moveCenterS
            onlyEvenPals
            input
            rightmost
            (currentPalindrome : maximalPalindromesIn)
            maximalPalindromesIn
            currentPalindrome
    | otherwise =
        -- the current palindrome can be extended
        extendPalindromeS
            onlyEvenPals
            input
            (rightmost + 1)
            maximalPalindromesIn
            (currentPalindrome + 2)
  where
    first = 0 -- first index of the input
    lastPos = G.length input - 1 -- last index of the input

-- | If the current palindrome cannot be extended anymore, this function will move the centers one step
moveCenterS
    :: (PalEq a, G.Vector v a)
    => Bool
    -- ^ indicates whether the input datatype is anti-reflexive
    -> v a
    -- ^ input, with only the elements we want to find palindromes in
    -> Int
    -- ^ the rightmost index which is checked by the algorithm
    -> [Int]
    -- ^ length of all maximal palindromes that are already found
    -> [Int]
    {- ^ length of maximal palindromes that are already found, where head is always the
    maximal palindrome at the 'mirrored' index
    -}
    -> Int
    -- ^ the number of centers moveCenterS still has to find maximal palindromes for
    -> [Int]
    -- ^ the final list of maximal palindrome lengths
moveCenterS
    antiReflexive
    input
    rightmost
    maximalPalindromesIn
    maximalPalindromesIn'
    nrOfCenters
        | nrOfCenters == 0 =
            -- the last centre is on the last element: try to extend the tail
            let (newPalLength, inputForExtend) =
                    case (antiReflexive, palEqToItselfAtIndex input rightmost) of
                        {- Non-anti-reflexive type, but element at `rightmost` is not
                         PalEq to itself. We found an empty maximal palindrome. Add it to
                         the list and continue searching. -}
                        (False, False) -> (0, 0 : maximalPalindromesIn)
                        {- Non-anti-reflexive type, element at `rightmost` is
                        PalEq to itself. Found a palindrome of at least length 1, so try to
                        extend it. -}
                        (False, True) -> (1, maximalPalindromesIn)
                        {- Anti-reflexive type: only centers between elements matter. We
                        assume an empty palindrome and continue extending. -}
                        (True, _) -> (0, maximalPalindromesIn)
            in  extendPalindromeS
                    antiReflexive
                    input
                    (rightmost + 1)
                    inputForExtend
                    newPalLength
        | otherwise =
            {- move the centres one step and add the length of the longest palindrome to
            the centres -}
            case maximalPalindromesIn' of
                (headq : tailq) ->
                    if headq == nrOfCenters - centerfactor
                        then
                            {- The previous element in the centre list reaches exactly to the end of the
                            last tail palindrome. Use the mirror property of palindromes to find the
                            longest tail palindrome -}
                            extendPalindromeS
                                antiReflexive
                                input
                                rightmost
                                maximalPalindromesIn
                                (nrOfCenters - centerfactor)
                        else
                            {- move the centres one step and add the length of the longest palindrome to
                            the centres -}
                            moveCenterS
                                antiReflexive
                                input
                                rightmost
                                (min headq (nrOfCenters - centerfactor) : maximalPalindromesIn)
                                tailq
                                (nrOfCenters - centerfactor)
                [] -> error "extendPalindromeS: empty sequence"
      where
        {- If type is anti-reflexive, we can skip centers on elements, so take steps of
        size 2. -}
        centerfactor
            | antiReflexive = 2
            | otherwise = 1

{- | After the current palindrome reached the end of the input vector, this function will
find and return the final palindromes using the pal in pal property.
-}
finalPalindromesS
    :: Bool
    -- ^ indicates whether the input datatype is anti-reflexive.
    -> Int
    -- ^ number of centers that haven't been extended yet.
    -> [Int]
    {- ^ list of found palindrome lengths, where the head corresponds to the value which
    must be copied next.
    -}
    -> [Int]
    {- ^ the lengths of all the palindromes that are found, including palindromes found by
    this function. This is used as an accumulator.
    -}
    -> [Int]
    -- ^ the lengths of all found maximal palindromes in reverse order.
finalPalindromesS antiReflexive nrOfCenters maximalPalindromesIn acc
    | nrOfCenters == 0 =
        acc
    | nrOfCenters > 0 =
        case maximalPalindromesIn of
            (p : ps) ->
                {- for a center, add the (truncated) copied palindrome to the accumulator
                and recurse over nrOfCenters. -}
                finalPalindromesS
                    antiReflexive
                    (nrOfCenters - centerfactor)
                    ps
                    {- the palindrome cannot be bigger than the remaining number of
                    centers minus the centerfactor. Truncate the palindrome if necessary.
                    -}
                    (min p (nrOfCenters - centerfactor) : acc)
            [] -> error "finalPalindromesS: empty sequence"
    | otherwise = error "finalPalindromesS: input < 0"
  where
    {- If type is anti-reflexive, we can skip centers on elements, so take steps of
    size 2. -}
    centerfactor
        | antiReflexive = 2
        | otherwise = 1
