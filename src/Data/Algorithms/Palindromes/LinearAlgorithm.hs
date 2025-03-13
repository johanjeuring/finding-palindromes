module Data.Algorithms.Palindromes.LinearAlgorithm
    ( extendPalindromeS
    , extendTailWord
    , finalPalindromesS
    , moveCenterS
    ) where

import Data.Algorithms.Palindromes.PalindromesUtils
    ( Couplable
    , surroundedByPunctuation
    , (=:=)
    )
import Data.Array (Array, (!))

import qualified Data.Vector as V

extendPalindromeS
    :: (Couplable a)
    => Int
    -- ^ centerfactor, the stepsize of the algorithm is 1 for text and 2 for DNA.
    -> Int
    -- ^ tailfactor, we are not sure whether this is needed anymore
    -> V.Vector a
    -- ^ input, with only the elements we want to find palindromes in
    -> [Int]
    -- ^ length of palindromes that are already found
    -> Int
    -- ^ the rightmost index which is checked by the algorithm
    -> Int
    -- ^ the length of the palindrome currently being expanded
    -> [Int]
    -- ^ the final list of palindrome lengths
extendPalindromeS centerfactor tailfactor input maximalPalindromesIn rightmost currentPalindrome
    | rightmost > lastPos =
        -- reached the end of the array
        finalPalindromesS
            centerfactor
            currentPalindrome
            maximalPalindromesIn
            ++ (currentPalindrome : maximalPalindromesIn)
    | rightmost - currentPalindrome == first
        || not ((input V.! rightmost) =:= (input V.! (rightmost - currentPalindrome - 1))) =
        -- the current palindrome extends to the start of the array, or it cannot be
        -- extended
        moveCenterS
            centerfactor
            tailfactor
            input
            rightmost
            (currentPalindrome : maximalPalindromesIn)
            maximalPalindromesIn
            currentPalindrome
    | otherwise =
        -- the current palindrome can be extended
        extendPalindromeS
            centerfactor
            tailfactor
            input
            maximalPalindromesIn
            (rightmost + 1)
            (currentPalindrome + 2)
  where
    first = 0 -- first index of the input
    lastPos = V.length input - 1 -- last index of the input

moveCenterS
    :: (Couplable a)
    => Int
    -> Int
    -> V.Vector a
    -> Int
    -> [Int]
    -> [Int]
    -> Int
    -> [Int]
moveCenterS
    centerfactor
    tailfactor
    input
    rightmost
    maximalPalindromesIn
    maximalPalindromesIn'
    nrOfCenters
        | nrOfCenters == 0 =
            -- the last centre is on the last element: try to extend the tail of length 1
            extendPalindromeS
                centerfactor
                tailfactor
                input
                maximalPalindromesIn
                (rightmost + 1)
                tailfactor
        | nrOfCenters - centerfactor == head maximalPalindromesIn' =
            {- the previous element in the centre list reaches exactly to the end of the
             last tail palindrome use the mirror property of palindromes to find the
             longest tail palindrome -}
            extendPalindromeS
                centerfactor
                tailfactor
                input
                maximalPalindromesIn
                rightmost
                (nrOfCenters - centerfactor)
        | otherwise =
            {- move the centres one step and add the length of the longest palindrome to
            the centres -}
            case maximalPalindromesIn' of
                (headq : tailq) ->
                    moveCenterS
                        centerfactor
                        tailfactor
                        input
                        rightmost
                        (min headq (nrOfCenters - centerfactor) : maximalPalindromesIn)
                        tailq
                        (nrOfCenters - centerfactor)
                [] -> error "extendPalindromeS: empty sequence"

{- | After the current palindrome reached the end of the input vector, this function will
find and return the final palindromes using the pal in pal property
-}
finalPalindromesS
    :: Int -- centerfactor
    -> Int -- amount of centers that haven't been extended before finalizing extendPalindromeS
    -> [Int] -- the lengths of the palindromes that are found, excluding the current palindrome
    -> [Int] -- the final sequence of found palindromes for each remaining center in reverse order
finalPalindromesS centerfactor nrOfCenters maximalPalindromesIn =
    -- Truncate the mirrored candidates when needed.
    zipWith min candidatesRev [0, centerfactor ..]
  where
    {- Candidates is the list of previously found palindromes we need to copy and possibly
    truncate -}
    candidates = take (nrOfCenters `div` centerfactor) maximalPalindromesIn
    {- We need to 'mirror' the candidates to get the remaining palindromes length in the
    right order.-}
    candidatesRev = reverse candidates

{-
---------------------------------------------------------------------
      Begin functions for finding punctuation palindromes,
      still called word palindromes here
---------------------------------------------------------------------
-}

extendTailWord
    :: V.Vector Char
    -> V.Vector Char
    -> Array Int Int
    -> [(Int, [Int])]
    -> Int
    -> (Int, [Int])
    -> [(Int, [Int])]
extendTailWord input textInput positionTextInput centres n current@(currentTail, currentTailWords)
    | n > alast =
        -- reached the end of the text input array
        finalWordCentres
            input
            textInput
            positionTextInput
            (current : centres)
            currentTail
            centres
            (1 + length centres)
    | n - currentTail == afirst =
        -- the current longest tail palindrome extends to the start of the text input array
        extendWordCentres
            input
            textInput
            positionTextInput
            (current : centres)
            n
            centres
            currentTail
    | (textInput V.! n) =:= (textInput V.! (n - currentTail - 1)) =
        -- the current longest tail palindrome can be extended
        -- check whether or not the extended palindrome is a wordpalindrome
        if surroundedByPunctuation
            (positionTextInput ! (n - currentTail - 1))
            (positionTextInput ! n)
            input
            then
                extendTailWord
                    input
                    textInput
                    positionTextInput
                    centres
                    (n + 1)
                    (currentTail + 2, currentTail + 2 : currentTailWords)
            else
                extendTailWord
                    input
                    textInput
                    positionTextInput
                    centres
                    (n + 1)
                    (currentTail + 2, currentTailWords)
    | otherwise =
        -- the current longest tail palindrome cannot be extended
        extendWordCentres
            input
            textInput
            positionTextInput
            (current : centres)
            n
            centres
            currentTail
  where
    (afirst, alast) = (0, V.length textInput - 1)

extendWordCentres
    :: V.Vector Char
    -> V.Vector Char
    -> Array Int Int
    -> [(Int, [Int])]
    -> Int
    -> [(Int, [Int])]
    -> Int
    -> [(Int, [Int])]
extendWordCentres input textInput positionTextInput centres n tcentres centreDistance
    | centreDistance == 0 =
        -- the last centre is on the last element:
        -- try to extend the tail of length 1
        if surroundedByPunctuation (positionTextInput ! n) (positionTextInput ! n) input
            then extendTailWord input textInput positionTextInput centres (n + 1) (1, [1, 0])
            else extendTailWord input textInput positionTextInput centres (n + 1) (1, [0])
    | centreDistance - 1 == fst (head tcentres) =
        -- the previous element in the centre list
        -- reaches exactly to the end of the last
        -- tail palindrome use the mirror property
        -- of palindromes to find the longest tail
        -- palindrome
        let (currentTail, oldWord : oldWords) = head tcentres
        in  if surroundedByPunctuation
                (positionTextInput ! (n - currentTail))
                (positionTextInput ! (n - 1))
                input
                then
                    if oldWord == currentTail
                        then extendTailWord input textInput positionTextInput centres n (head tcentres)
                        else
                            extendTailWord
                                input
                                textInput
                                positionTextInput
                                centres
                                n
                                (currentTail, currentTail : oldWord : oldWords)
                else
                    if oldWord == currentTail && oldWord > 0
                        then
                            extendTailWord
                                input
                                textInput
                                positionTextInput
                                centres
                                n
                                (currentTail, tail (snd (head tcentres)))
                        else extendTailWord input textInput positionTextInput centres n (head tcentres)
    | otherwise =
        -- move the centres one step
        -- add the length of the longest palindrome
        -- to the centres
        let newTail = min (fst (head tcentres)) (centreDistance - 1)
            oldWord = head (snd (head tcentres))
            newWords
                | oldWord < newTail =
                    if surroundedByPunctuation
                        (positionTextInput ! (n - newTail + 1))
                        (positionTextInput ! n)
                        input
                        then newTail : snd (head tcentres)
                        else snd (head tcentres)
                | null (tail (snd (head tcentres))) =
                    snd (head tcentres)
                | otherwise =
                    tail (snd (head tcentres))
        in  extendWordCentres
                input
                textInput
                positionTextInput
                ((newTail, newWords) : centres)
                n
                (tail tcentres)
                (centreDistance - 1)

finalWordCentres
    :: V.Vector Char
    -> V.Vector Char
    -> Array Int Int
    -> [(Int, [Int])]
    -> Int
    -> [(Int, [Int])]
    -> Int
    -> [(Int, [Int])]
finalWordCentres input textInput positionTextInput centres n tcentres mirrorPoint
    | n == 0 = centres
    | n > 0 =
        let tlast = V.length textInput - 1
            (oldTail, oldWord : oldWords) = head tcentres
            newTail = min oldTail (n - 1)
            newWord = min oldWord (n - 1)
            tailFirstMirror = min tlast (div (mirrorPoint - newTail) 2)
            tailLastMirror =
                min
                    tlast
                    (if odd newTail then div (mirrorPoint + newTail) 2 else div (mirrorPoint + newTail) 2 - 1)
            wordFirstMirror = min tlast (div (mirrorPoint - newWord) 2)
            wordLastMirror =
                min
                    tlast
                    (if odd newWord then div (mirrorPoint + newTail) 2 else div (mirrorPoint + newTail) 2 - 1)
            newWords
                | surroundedByPunctuation
                    (positionTextInput ! tailFirstMirror)
                    (positionTextInput ! tailLastMirror)
                    input =
                    if newWord == newTail
                        then newTail : oldWords
                        else newTail : oldWord : oldWords
                | surroundedByPunctuation
                    (positionTextInput ! wordFirstMirror)
                    (positionTextInput ! wordLastMirror)
                    input
                    || null oldWords =
                    newWord : oldWords
                | otherwise = oldWords
        in  finalWordCentres
                input
                textInput
                positionTextInput
                ((newTail, newWords) : centres)
                (n - 1)
                (tail tcentres)
                (mirrorPoint + 1)
    | otherwise = error "finalWordCentres: input < 0"

{-
---------------------------------------------------------------------
      End functions for finding punctuation palindromes
---------------------------------------------------------------------
-}
