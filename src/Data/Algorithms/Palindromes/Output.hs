-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- \|
-- Module      :  Data.Algorithms.Palindromes.Output
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
module Data.Algorithms.Palindromes.Output
    ( indicesInOutputText
    , indicesInOutputWord
    , indicesToText
    , longestLength
    , longestWord
    , allLengths
    , allWords
    , lengthAt
    , wordAt
    ) where

import Data.Algorithms.Palindromes.PalindromesUtils
    ( Palindrome (..)
    )
import Data.Algorithms.Palindromes.PreProcessing
    ( filterLetters'
    )
import Data.Char
    ( isLetter
    , isSpace
    )

import qualified Data.Vector as V

{- | Takes a start and an end index in the filtered string and returns the indices
in the unfiltered string
-}
indicesInOutputText :: (Int, Int) -> String -> (Int, Int)
indicesInOutputText (start', end') input
    | start' >= length originalIndices = (length input, length input)
    | end' - start' > 0 = (start, end)
    | otherwise = (start, start)
  where
    originalIndices = filterLetters' input
    start = fst $ originalIndices V.! start'
    end = fst (originalIndices V.! (end' - 1)) + 1

{- | Takes a start and end index in the list of words and returns the start and end indices
of the text of the word palindrome in the original string
-}
indicesInOutputWord :: (Int, Int) -> String -> (Int, Int)
indicesInOutputWord (start', end') input
    | start' >= length wordLengths =
        (length input, length input)
    | end' - start' > 0 = (start, end)
    | otherwise = (start, start)
  where
    start = startSpaces + otherAtFiltered V.! startSpaces
    end = endSpaces + otherAtFiltered V.! (endSpaces - 1)

    startSpaces :: Int
    startSpaces = startIndices' V.! start' + spaces V.! start'
    endSpaces :: Int
    endSpaces = (endIndices' V.! (end' - 1) + spaces V.! (end' - 1)) + 1

    startIndices' :: V.Vector Int
    startIndices' = V.fromList $ init $ scanl (+) 0 wordLengths
    endIndices' :: V.Vector Int
    endIndices' = V.fromList $ drop 1 $ scanl (+) (-1) wordLengths

    spaces :: V.Vector Int
    spaces = V.map snd $ V.filter isSpaceCount $ V.indexed spacesAt
    wordLengths :: [Int]
    wordLengths = map length $ words $ filter (\a -> isLetter a || isSpace a) input
    spacesAt :: V.Vector Int
    spacesAt = V.fromList $ drop 1 $ scanl (\acc x -> if isSpace x then acc + 1 else acc) 0 input

    otherAtFiltered :: V.Vector Int
    otherAtFiltered =
        if V.null $
            V.map fst $
                V.filter
                    (\(_, c) -> isLetter c || isSpace c)
                    (V.zip otherAt $ V.fromList input)
            then V.fromList [0]
            else
                V.map fst $
                    V.filter
                        (\(_, c) -> isLetter c || isSpace c)
                        (V.zip otherAt $ V.fromList input)
    otherAt :: V.Vector Int
    otherAt =
        V.fromList $
            drop 1 $
                scanl (\acc x -> if (not . isLetter) x && (not . isSpace) x then acc + 1 else acc) 0 input

    isSpaceCount :: (Int, Int) -> Bool
    isSpaceCount (i, w) =
        ( i /= 0
            && i /= length spacesAt - 1
            && spacesAt V.! (i - 1) == w
            && spacesAt V.! (i + 1) /= w
        )
            || i == length spacesAt - 1
            || ( i == 0
                    && w == 0
                    && spacesAt V.! (i + 1) /= w
               )

-- | Takes a start and end index (exclusive) and returns the substring with those indices
indicesToText :: (Int, Int) -> String -> String
indicesToText (start, end) input
    | end - start > 0 = take (end - start) $ drop start input
    | otherwise = ""

-- | Returns the length of the longest palindrome as a string
longestLength :: [Int] -> String
longestLength = show . maximum

-- | Converts the longest palindrome to text
longestWord :: [Palindrome] -> String
longestWord input = palText longest
  where
    longest :: Palindrome
    longest = foldr1 (\a b -> if palLength a < palLength b then b else a) input

-- | All maximal palindrome lengths
allLengths :: [Int] -> String
allLengths = show

-- | All maximal palindromes as a list of text
allWords :: [Palindrome] -> String
allWords input = show $ map palText input

lengthAt :: Int -> [Int] -> String
lengthAt n lengths = show $ lengths !! n

wordAt :: Int -> [Palindrome] -> String
wordAt n pals = palText $ pals !! n