{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}

{- |
Module      :  Data.Algorithms.Palindromes.Finders
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

This module is the core of this package and contains the functions that find palindromes.
Most useful perhaps is the findPalindromesFormatted which also formats the palindrome
based on the outputFormat data type as described in this module. For more statistics the
findPalindromes which outputs a palindrome can be used.
-}
module Data.Algorithms.Palindromes.Finders
    ( findPalindromes
    , findPalindromeRanges
    , findPalindromesFormatted
    , formatPalindromes
    , Variant (..)
    , OutputFormat (..)
    , Complexity (..)
    , filterFunctionsPalindromes
    , filterPalindromes
    ) where

import Data.List (foldl')

import Data.Algorithms.Palindromes.Algorithms
    ( insertionDeletionAlgorithm
    , linearAlgorithm
    , quadraticAlgorithm
    )
import Data.Algorithms.Palindromes.Output
    ( indicesInOutputText
    , indicesInOutputWord
    , lengthAt
    , longest
    , rangeToText
    , showLengths
    , showTexts
    , wordAt
    )
import Data.Algorithms.Palindromes.PalEq (PalEq)
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..), getLength)
import Data.Algorithms.Palindromes.PostProcessing (filterPunctuation)
import Data.Algorithms.Palindromes.PreProcessing
    ( filterLetters
    , filterLetters'
    , textToWords
    , textToWordsWithIndices
    , tryParseDNA
    )
import Data.Algorithms.Palindromes.RangeFunctions
    ( indexedLengthToRange
    , rangeToLength
    )

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

{- | Used as a setting for palindrome finding functions. This describes the kind of
palindrome we want to find.
-}
data Variant
    = -- | Convert the text to DNA, then match A with T and G with C.
      VarDNA
    | {- | Find text palindromes, then shrink each palindrome until it is surrounded by
      punctuation so that no palindrome can contain only part of a word.
      -}
      VarPunctuation
    | -- | Find palindromes only based on letters, ignore all other characters.
      VarText
    | -- | Find palindromes in the text exactly as it was given.
      VarPlain
    | -- | Compare words instead of individual characters to look for palindromes.
      VarWord
    deriving (Show)

{- | Used to describe different possible output formats of palindromes. Used as a setting
in finding functions.
-}
data OutputFormat
    = -- | Output the length of the longest palindrome
      OutLength
    | -- | Output all longest palindromes of same size as text
      OutWord
    | -- | Output the lengths of all maximal palindromes around each center
      OutLengths
    | -- | Output all maximal palindromes around each center as text
      OutWords
    | -- | Output the length of the palindrome at a certain center index
      OutLengthAt Int
    | -- | Output the palindrome at a certain center index as text
      OutWordAt Int
    deriving (Show)

{- | Used as a setting for what algorithm to run. The quadratic algorithm also has
functionality for including gaps and errors, therefore this is given as an extra setting.
-}
data Complexity
    = ComLinear
    | ComQuadratic {gapSize :: Int, maxError :: Int}
    | ComInsertionDeletion {gapSizeID :: Int, maxIDError :: Int}
    deriving (Show)

{- This method returns whether uneven palindromes are impossible to exist based on the
query settings. -}
onlyEvenPals :: Variant -> Complexity -> Bool
onlyEvenPals VarDNA (ComQuadratic gapSize _) = even gapSize
onlyEvenPals VarDNA ComLinear = True
onlyEvenPals _ _ = False

{- | This function combines three phases based on the settings and input given: The
pre-processing phase, the algorithm phase and the post-processing phase. It finds and
returns a list of (Int, Int), which represents the range of every found palindrome in the input.
-}
findPalindromeRanges
    :: Variant -> Complexity -> U.Vector Char -> [(Int, Int)]
findPalindromeRanges variant complexity input =
    (post . preAlg) input
  where
    {- The pre-processing phase parses the text input based on the Variant provided to a
    vector of PalEq items. -}
    preAlg :: U.Vector Char -> [(Int, Int)]
    preAlg = case variant of
        VarText -> alg . filterLetters
        VarPunctuation -> alg . filterLetters
        VarDNA -> alg . tryParseDNA
        VarWord -> alg . textToWords
        _ -> alg

    {- The algorithm phase runs one of the algorithms that finds the ranges, since the linear and quadratic
    find indexLists we must convert these to ranges. -}
    alg :: (PalEq b, G.Vector v b) => v b -> [(Int, Int)]
    alg = case complexity of
        ComLinear -> indexListToRanges . linearAlgorithm (onlyEvenPals variant complexity)
        ComQuadratic gapSize errors ->
            indexListToRanges
                . quadraticAlgorithm
                    (onlyEvenPals variant complexity)
                    gapSize
                    errors
        ComInsertionDeletion gapSize errors -> insertionDeletionAlgorithm gapSize errors

    indexListToRanges :: [Int] -> [(Int, Int)]
    indexListToRanges = go 0
      where
        go !_ [] = []
        go !i (x : xs) = indexedLengthToRange (i, x) : go (i + increment) xs
        increment = if onlyEvenPals variant complexity then 2 else 1

    {- The post-processing phase changes the list of ranges so that they fit the
    requirements in the case of punctuation palindromes -}
    post :: [(Int, Int)] -> [(Int, Int)]
    post = case variant of
        VarPunctuation -> filterPunctuation input
        _ -> id

{- | This function combines all the phases required to find palindromes.
It first finds all the palindrome ranges based on the settings,
then filters them by length and finally converts the found ranges to the Palindrome datatype
-}
findPalindromes :: Variant -> Complexity -> Int -> String -> [Palindrome]
findPalindromes variant complexity minlen input =
    map rangeToPalindrome $ filterRanges $ findPalindromeRanges variant complexity inputVector
  where
    rangeToPalindrome :: (Int, Int) -> Palindrome
    rangeToPalindrome r =
        Palindrome
            { palRange = r
            , palText = rangeToText (indicesInOriginal r) inputVector
            , palRangeInText = indicesInOriginal r
            }

    filterRanges :: [(Int, Int)] -> [(Int, Int)]
    filterRanges = filter ((>= minlen) . rangeToLength)

    -- Takes a (start character index, end character index) pair. These character indeces are in the original (not pre-processed)
    indicesInOriginal :: (Int, Int) -> (Int, Int)
    indicesInOriginal range = case variant of
        VarText -> indicesInOutputText range inputLength (filterLetters' inputVector)
        VarPunctuation -> indicesInOutputText range inputLength (filterLetters' inputVector)
        VarDNA -> indicesInOutputText range inputLength (filterLetters' inputVector)
        VarPlain -> range
        VarWord -> indicesInOutputWord range inputLength (textToWordsWithIndices inputVector)
    !inputVector = U.fromList input
    !inputLength = U.length inputVector

{- | This function combines four phases based on the settings and input given: The
pre-processing, the algorithm phase, the post processing phase, the parsing phase and the
output phase. The final phase takes the OutputFormat flag into account and returns a
String that can be printed. It return the palindrome found using the settings, formatted
to the given outputFormat.
-}
findPalindromesFormatted
    :: Variant -> OutputFormat -> Complexity -> Int -> String -> String
findPalindromesFormatted variant outputFormat complexity minlen input =
    formatPalindromes outputFormat $
        filterPalindromes outputFormat $
            findPalindromes variant complexity minlen input

filterPalindromes :: OutputFormat -> [Palindrome] -> [Palindrome]
filterPalindromes outputFormat = case filterFunctionsPalindromes outputFormat of
    Nothing -> id
    Just foldF -> reverse . foldl' foldF []

filterFunctionsPalindromes
    :: OutputFormat
    -> Maybe
        ([Palindrome] -> Palindrome -> [Palindrome])
filterFunctionsPalindromes outputFormat = case outputFormat of
    OutLength -> Just longest
    OutWord -> Just longest
    OutLengths -> Nothing
    OutWords -> Nothing
    OutLengthAt _ -> Nothing
    OutWordAt _ -> Nothing

formatPalindromes :: OutputFormat -> [Palindrome] -> String
formatPalindromes _ [] = "No palindromes found"
formatPalindromes outputFormat pals = case outputFormat of
    OutLength -> case lengths of
        [] -> undefined
        x : _ -> show x
    OutWord -> showTexts pals
    OutLengths -> showLengths lengths
    OutWords -> showTexts pals
    OutLengthAt x -> lengthAt x lengths
    OutWordAt x -> wordAt x pals
  where
    lengths = map getLength pals
