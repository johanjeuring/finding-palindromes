{-# LANGUAGE MonoLocalBinds #-}

module Data.Algorithms.Palindromes.LinearAlgorithm
    ( extendPalindromeS
    , finalPalindromesS
    , moveCenterS
    ) where

import Data.Algorithms.Palindromes.Couplable (Couplable (..), couplableWithItselfAtIndex)

import qualified Data.Vector as V

-- | This function will traverse an input linearly, using an accumulator
extendPalindromeS
    :: (Couplable a)
    => Bool
    -- ^ Indicates whether the input datatype is anti-reflexive
    -> V.Vector a
    -- ^ input, with only the elements we want to find palindromes in
    -> Int
    -- ^ the rightmost index which is checked by the algorithm
    -> [Int]
    -- ^ length of palindromes that are already found
    -> Int
    -- ^ the length of the palindrome currently being expanded
    -> [Int]
    -- ^ the final list of maximal palindrome lengths
extendPalindromeS antiReflexive input rightmost maximalPalindromesIn currentPalindrome
    | rightmost > lastPos =
        -- reached the end of the array
        finalPalindromesS
            antiReflexive
            currentPalindrome
            maximalPalindromesIn
            ++ (currentPalindrome : maximalPalindromesIn)
    | rightmost - currentPalindrome == first
        || not ((input V.! rightmost) =:= (input V.! (rightmost - currentPalindrome - 1))) =
        -- the current palindrome extends to the start of the array, or it cannot be
        -- extended
        moveCenterS
            antiReflexive
            input
            rightmost
            (currentPalindrome : maximalPalindromesIn)
            maximalPalindromesIn
            currentPalindrome
    | otherwise =
        -- the current palindrome can be extended
        extendPalindromeS
            antiReflexive
            input
            (rightmost + 1)
            maximalPalindromesIn
            (currentPalindrome + 2)
  where
    first = 0 -- first index of the input
    lastPos = V.length input - 1 -- last index of the input

-- | If the current palindrome cannot be extended anymore, this function will move the centers one step
moveCenterS
    :: (Couplable a)
    => Bool
    -- ^ Indicates whether the input datatype is anti-reflexive
    -> V.Vector a
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
                    case (antiReflexive, couplableWithItselfAtIndex input rightmost) of
                        {- Non-anti-reflexive type, but element at `rightmost` is not
                         self-couplable. We found an empty maximal palindrome. Add it to
                         the list and continue searching. -}
                        (False, False) -> (0, 0 : maximalPalindromesIn)
                        {- Non-anti-reflexive type, element at `rightmost` is
                        self-couplable. Found a palindrome of at least length 1, so try to
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
        centerfactor = if antiReflexive then 2 else 1

{- | After the current palindrome reached the end of the input vector, this function will
find and return the final palindromes using the pal in pal property
-}
finalPalindromesS
    :: Bool
    -- ^ Indicates whether the input datatype is anti-reflexive
    -> Int
    -- ^ amount of centers that haven't been extended before finalizing extendPalindromeS
    -> [Int]
    -- ^ the lengths of the palindromes that are found, excluding the current palindrome
    -> [Int]
    -- ^ the final sequence of found palindromes for each remaining center in reverse order
finalPalindromesS antiReflexive nrOfCenters maximalPalindromesIn =
    -- Truncate the mirrored candidates when needed.
    zipWith min candidatesRev [0, centerfactor ..]
  where
    {- Candidates is the list of previously found palindromes we need to copy and possibly
    truncate -}
    candidates = take (nrOfCenters `div` centerfactor) maximalPalindromesIn
    {- We need to 'mirror' the candidates to get the remaining palindromes length in the
    right order.-}
    candidatesRev = reverse candidates
    {- If type is anti-reflexive, we can skip centers on elements, so take steps of
    size 2. -}
    centerfactor = if antiReflexive then 2 else 1
