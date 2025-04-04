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

import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (..)
    )
import Data.Algorithms.Palindromes.PreProcessing
    ( filterLetters'
    )
import Data.Char
    ( isAlphaNum
    , isSpace
    )
import Data.List (intercalate)

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
    -- The start and end index of the found palindrome in the original string
    start = startSpaces + otherAtFiltered V.! startSpaces
    end = endSpaces + otherAtFiltered V.! (endSpaces - 1)

    -- The number of spaces found before the start index
    startSpaces :: Int
    startSpaces = startIndices' V.! start' + spaces V.! start'
    -- The number of spaces found before the end index
    endSpaces :: Int
    endSpaces = (endIndices' V.! (end' - 1) + spaces V.! (end' - 1)) + 1

    -- The number of spaces found before the start of each word
    startIndices' :: V.Vector Int
    startIndices' = V.fromList $ init $ scanl (+) 0 wordLengths
    -- The number of spaces found before the end of each word
    endIndices' :: V.Vector Int
    endIndices' = V.fromList $ drop 1 $ scanl (+) (-1) wordLengths

    -- The number of consecutive spaces found from the previous word to the next word
    spaces :: V.Vector Int
    spaces = V.map snd $ V.filter isSpaceCount $ V.indexed spacesAt

    -- The lengths of all words in the input string, including only letters and numbers
    wordLengths :: [Int]
    wordLengths = map length $ words $ filter (\a -> isAlphaNum a || isSpace a) input
    -- The number of spaces until any index in the original input string
    spacesAt :: V.Vector Int
    spacesAt =
        V.fromList $
            drop 1 $
                scanl
                    (\acc x -> if isSpace x then acc + 1 else acc)
                    0
                    (filter (\x -> isAlphaNum x || isSpace x) input)

    -- The number of "other" characters before every word
    --   "other" is defined as any character that is not alphanumeric or whitespace
    otherAtFiltered :: V.Vector Int
    otherAtFiltered =
        if V.null $
            V.map fst $
                V.filter
                    (\(_, c) -> isAlphaNum c || isSpace c)
                    (V.zip otherAt $ V.fromList input)
            then V.fromList [0]
            else
                V.map fst $
                    V.filter
                        (\(_, c) -> isAlphaNum c || isSpace c)
                        (V.zip otherAt $ V.fromList input)
    -- The number of "other" characters before every input character
    otherAt :: V.Vector Int
    otherAt =
        V.fromList $
            drop 1 $
                scanl
                    (\acc x -> if (not . isAlphaNum) x && (not . isSpace) x then acc + 1 else acc)
                    0
                    input

    -- A number at an index is a "space count" when it's a number that marks the number of spaces
    --   belonging to a single space between two words.
    -- Example:
    -- The string "  . q we r" has the spaces
    -- [1,2,2,3,3,4,4,4,5,5]
    --          ^     ^   ^
    -- The number 3, 4 and 5 mark the number of spaces before q, we and r respectively.
    isSpaceCount :: (Int, Int) -> Bool
    isSpaceCount (spaceIndex, numberOfSpaces) =
        ( spaceIndex /= 0
            && spaceIndex /= length spacesAt - 1
            && spacesAt V.! (spaceIndex - 1) == numberOfSpaces
            && spacesAt V.! (spaceIndex + 1) /= numberOfSpaces
        )
            || spaceIndex == length spacesAt - 1
            || ( spaceIndex == 0
                    && numberOfSpaces == 0
                    && spacesAt V.! (spaceIndex + 1) /= numberOfSpaces
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

{- | All maximal palindromes as a list of text
Same as show $ map palText input except this doesn't apply show to the palindrome strings as that will turn \n into \\n
-}
allWords :: [Palindrome] -> String
allWords input = "[" ++ intercalate "," (map (\x -> "\"" ++ palText x ++ "\"") input) ++ "]"

lengthAt :: Int -> [Int] -> String
lengthAt n lengths = show $ lengths !! n

wordAt :: Int -> [Palindrome] -> String
wordAt n pals = palText $ pals !! n