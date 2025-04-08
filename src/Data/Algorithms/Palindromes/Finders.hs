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
    , findPalindromeLengths
    , findPalindromesFormatted
    , Variant (..)
    , OutputFormat (..)
    , Complexity (..)
    , LengthMod
    ) where

import Data.Algorithms.Palindromes.Algorithms
    ( linearAlgorithm
    , quadraticAlgorithm
    )
import Data.Algorithms.Palindromes.DNA (DNA)
import Data.Algorithms.Palindromes.Output
    ( allLengths
    , allWords
    , indicesInOutputText
    , indicesInOutputWord
    , indicesToText
    , lengthAt
    , longestLength
    , longestWord
    , wordAt
    )
import Data.Algorithms.Palindromes.PalEq (PalEq)
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..))
import Data.Algorithms.Palindromes.PostProcessing
    ( filterMax
    , filterMin
    , filterPunctuation
    )
import Data.Algorithms.Palindromes.PreProcessing
    ( filterLetters
    , textToDNA
    , textToWords
    )
import Data.Algorithms.Palindromes.RangeFunctions (indexedLengthToRange)
import Data.Maybe (fromJust, isNothing)

import qualified Data.Vector as V

{- | Used as a setting for palindrome finding functions. This describes the kind of
palindrome we want to find.
-}
data Variant
    = -- | Convert the text to DNA, then match A with T and G with C.
      VarDNA
    | -- | Find text palindromes, then shrink each palindrome until it is surrounded by
      --       punctuation so that no palindrome can contain only part of a word.
      VarPunctuation
    | -- | Find palindromes only based on letters, ignore all other characters.
      VarText
    | -- | Find palindromes in the text exactly as it was given.
      VarPlain
    | -- | Compare words instead of individual characters to look for palindromes.
      VarWord

{- | Used to describe different possible output formats of palindromes. Used as a setting
in finding functions.
-}
data OutputFormat
    = -- | Output the length of the longest palindrome
      OutLength
    | -- | Output longest palindrome as text
      OutWord
    | -- | Output the lengths of all maximal palindromes around each center
      OutLengths
    | -- | Output all maximal palindromes around each center as text
      OutWords
    | -- | Output the length of the palindrome at a certain center index
      OutLengthAt Int
    | -- | Output the palindrome at a certain center index as text
      OutWordAt Int

{- | Used as a setting for what algorithm to run. The quadratic algorithm also has
functionality for including gaps and errors, therefore this is given as an extra setting.
-}
data Complexity
    = ComLinear
    | ComQuadratic {gapSize :: Int, maxError :: Int}

-- | The minimum and maximum length of the palindromes you want to find.
type LengthMod = (Int, Maybe Int)

{- | This function combines three phases based on the settings and input given: The
pre-processing phase, the algorithm phase and the post-processing phase. It finds and
returns a list of integers, which corresponds to the lengths of the maximal palindromes
around each center.
-}
findPalindromeLengths
    :: Variant -> Complexity -> LengthMod -> String -> [Int]
findPalindromeLengths variant complexity (minlength, maxlength') input =
    (post . preAlg) input
  where
    {- The pre-processing phase parses the text input based on the Variant provided to a
    vector of PalEq items. -}
    preAlg :: String -> [Int]
    preAlg = case variant of
        VarText -> alg . filterLetters
        VarPunctuation -> alg . filterLetters
        VarDNA -> alg . tryParse
        VarWord -> alg . textToWords
        _ -> alg . V.fromList

    -- If trying to parse the string to DNA would fail, throw a more readable error
    tryParse :: String -> V.Vector DNA
    tryParse x
        | (isNothing . parseDna) x = error "Invalid DNA string"
        | otherwise = (fromJust . parseDna) x
    parseDna :: String -> Maybe (V.Vector DNA)
    parseDna = textToDNA . V.toList . filterLetters

    {- The algorithm phase runs one of the algorithms that finds the maximal palindromes
    around all centers. -}
    alg :: (PalEq b) => V.Vector b -> [Int]
    alg = case complexity of
        ComLinear -> linearAlgorithm isAntiReflexive
        _ ->
            quadraticAlgorithm
                (isAntiReflexive && even (gapSize complexity))
                (gapSize complexity)
                (maxError complexity)

    {- We distinguish this case since for anti reflexive coupleables (only DNA currently)
    we only need to run on even indices saving time and space. -}
    isAntiReflexive :: Bool
    isAntiReflexive = case variant of
        VarDNA -> True
        _ -> False

    {- The post-processing phase changes the list of centers so that all lengths fit the
    requirements, such as shrinking the sizes so that the palindrome is surrounded by
    punctuation, applying a minimum length and applying a maximum length. -}
    post :: [Int] -> [Int]
    post = case variant of
        VarPunctuation ->
            filterMin minlength
                . filterMax maxlength
                . filterPunctuation input
        _ -> filterMin minlength . filterMax maxlength
    maxlength :: Int
    maxlength
        | isNothing maxlength' = length input
        | otherwise = fromJust maxlength'

{- | This function combines four phases based on the settings and input given: The
pre-processing phase, the algorithm phase, the post-processing phase and the parsing
phase. The final phase parses the [Int] to a [Palindrome]. The function returns a list of
the data type Palindrome with a palindrome at each center index.
-}
findPalindromes :: Variant -> Complexity -> LengthMod -> String -> [Palindrome]
findPalindromes variant complexity lengthmod input = map lengthToPalindrome lengths
  where
    lengthToPalindrome :: (Int, Int) -> Palindrome
    lengthToPalindrome (index, len) =
        Palindrome
            { palCenterIndex = index
            , palLength = len
            , palText = indicesToText (indicesInOriginal (index, len)) input
            , palRange = indicesInOriginal (index, len)
            }

    {- A list of tuples containing the center index and the length of the maximal
    palindrome. -}
    lengths :: [(Int, Int)]
    lengths = zip [0 ..] $ findPalindromeLengths variant complexity lengthmod input

    {- A function that converts a (center index, length) pair to a (start index, end
    index) pair. -}
    indicesInOriginal :: (Int, Int) -> (Int, Int)
    indicesInOriginal il@(i, l) = case variant of
        VarText -> indicesInOutputText range input
        VarPunctuation -> indicesInOutputText range input
        VarDNA -> indicesInOutputText (dnaRange complexity) input
        VarPlain -> range
        VarWord -> indicesInOutputWord range input
      where
        range :: (Int, Int)
        range = indexedLengthToRange il
        dnaRange :: Complexity -> (Int, Int)
        dnaRange ComLinear = (i - (l `div` 2), i + (l `div` 2))
        dnaRange (ComQuadratic gap _) = if even gap then dnaRange ComLinear else range

{- | This function combines four phases based on the settings and input given: The
pre-processing, the algorithm phase, the post processing phase, the parsing phase and the
output phase. The final phase takes the OutputFormat flag into account and returns a
String that can be printed. It return the palindrome found using the settings, formatted
to the given outputFormat.
-}
findPalindromesFormatted
    :: Variant -> OutputFormat -> Complexity -> LengthMod -> String -> String
findPalindromesFormatted variant outputFormat complexity lengthmod input = text
  where
    result :: [Palindrome]
    result = findPalindromes variant complexity lengthmod input
    lengths :: [Int]
    lengths = findPalindromeLengths variant complexity lengthmod input
    text :: String
    text = case outputFormat of
        OutLength -> longestLength lengths
        OutWord -> longestWord result
        OutLengths -> allLengths lengths
        OutWords -> allWords result
        OutLengthAt x -> lengthAt x lengths
        OutWordAt x -> wordAt x result