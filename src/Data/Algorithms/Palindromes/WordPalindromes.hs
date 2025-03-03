-----------------------------------------------------------------------------
--
-- Module      :  Data.Algorithms.Palindromes.WordPalindromes
-- Copyright   :  (c) 2012 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Algorithms.Palindromes.WordPalindromes
    ( longestWordPalindrome
    , maximalWordPalindromesLengthAtLeast
    ) where

import Data.Algorithms.Palindromes.PalindromesUtils
import Data.Array (Array (), (!))
import Data.List (intercalate, maximumBy)

import qualified Data.ByteString as B

-----------------------------------------------------------------------------
-- longestWordPalindromes
-----------------------------------------------------------------------------

{- | longestWordPalindromes returns the longest word palindrome around each
  position in a string. The integer argument is used to only show
  palindromes of length at least this integer.
-}
maximalWordPalindromesLengthAtLeast :: Int -> B.ByteString -> String
maximalWordPalindromesLengthAtLeast m input =
    let textInput = B.map myToLower (B.filter myIsLetterW input)
        positionTextInput = listArrayl0 (B.findIndices myIsLetterW input)
    in  intercalate "\n" $
            maximalWordPalindromesLengthAtLeastBS m input textInput positionTextInput

maximalWordPalindromesLengthAtLeastBS
    :: Int -> B.ByteString -> B.ByteString -> Array Int Int -> [String]
maximalWordPalindromesLengthAtLeastBS m input textInput positionTextInput =
    map (showTextPalindrome input positionTextInput) $
        filter ((m <=) . fst) $
            zip (wordPalindromesAroundCentres input textInput positionTextInput) [0 ..]

{- | longestWordPalindrome returns the longest text palindrome preceded and
  followed by non-letter symbols (if any).
-}
longestWordPalindrome :: B.ByteString -> String
longestWordPalindrome input =
    let textInput = B.map myToLower (B.filter myIsLetterW input)
        positionTextInput = listArrayl0 (B.findIndices myIsLetterW input)
        (len, pos) =
            maximumBy
                (\(w, _) (w', _) -> compare w w')
                (zip (wordPalindromesAroundCentres input textInput positionTextInput) [0 ..])
    in  showTextPalindrome input positionTextInput (len, pos)

-----------------------------------------------------------------------------
-- wordPalindromesAroundCentres
--
-- This is the function palindromesAroundCentres, extended with the longest
-- word palindromes around each centre.
-----------------------------------------------------------------------------

{- | wordPalindromesAroundCentres returns the same lengths of palindromes as
  palindromesAroundCentres, but at the same time also the length of the
  longest word palindromes around the centres.
-}
wordPalindromesAroundCentres :: B.ByteString -> B.ByteString -> Array Int Int -> [Int]
wordPalindromesAroundCentres input textInput positionTextInput =
    let tfirst = 0
    in  reverse $
            map (head . snd) $
                extendTailWord input textInput positionTextInput [] tfirst (0, [0])

-- extendTailWordold textInput positionTextInput input n current centres = extendTailWord input textInput positionTextInput centres n current

extendTailWord
    :: B.ByteString
    -> B.ByteString
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
    | B.index textInput n == B.index textInput (n - currentTail - 1) =
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
    (afirst, alast) = (0, B.length textInput - 1)

extendWordCentres
    :: B.ByteString
    -> B.ByteString
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
    :: B.ByteString
    -> B.ByteString
    -> Array Int Int
    -> [(Int, [Int])]
    -> Int
    -> [(Int, [Int])]
    -> Int
    -> [(Int, [Int])]
finalWordCentres input textInput positionTextInput centres n tcentres mirrorPoint
    | n == 0 = centres
    | n > 0 =
        let tlast = B.length textInput - 1
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
