{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}

{- |
Module      :  Data.Algorithms.Palindromes.Finders
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

This module is the core of this package and contains the functions to find palindromes in text.
It defines the different options that can be used to find palindromes:
- The palindrome variant.
- The output format.
- The output filter.
- The algorithm to use.
-}
module Data.Algorithms.Palindromes.Finders
    ( Variant (..)
    , OutputFormat (..)
    , OutputFilter (..)
    , Algorithm (..)
    , findPalindromes
    , findPalindromesFormatted
    , formatPalindromes
    , filterPalindromes
    ) where

-- This import might throw a compiler warnings on GHC version 9.12 or higher,
--    but the import is necessary on older versions.
import Data.List (foldl')

import Data.Algorithms.Palindromes.Algorithms
    ( approximateAlgorithm
    , linearAlgorithm
    , quadraticAlgorithm
    )
import Data.Algorithms.Palindromes.Internal.Output
    ( indicesInOutputText
    , indicesInOutputWord
    , longest
    , rangeToText
    , showAll
    , showLengths
    , showRanges
    , showTexts
    )
import Data.Algorithms.Palindromes.Internal.PostProcessing (filterPunctuation)
import Data.Algorithms.Palindromes.Internal.PreProcessing
    ( filterLetters
    , filterLetters'
    , textToWordIndices
    , textToWords
    , tryParseDNA
    )
import Data.Algorithms.Palindromes.Internal.RangeFunctions
    ( Range
    , indexedLengthToRange
    , rangeToLength
    , rangeToPalindromeCenter
    )
import Data.Algorithms.Palindromes.PalEq (PalEq)
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..))

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
    deriving (Eq, Show)

{- | Used to describe different possible output formats of palindromes. Used as a setting
in finding functions.
-}
data OutputFormat
    = -- | Output the length of the palindromes
      FormatLength
    | -- | Output palindrome as text
      FormatText
    | -- | Output the ranges of the palindromes
      FormatRange
    | -- | Output all details: Text, range and length
      FormatAllDetails
    deriving (Eq, Show)

{- | Used to describe different possible filters on the output. Used as a setting in
finding functions.
-}
data OutputFilter
    = -- | Select longest (can be multiple of same length)
      SelectLongest
    | -- | Select to only keep palindromes with certain center position
      SelectAt Int
    | -- | Select all palindromes
      SelectAll
    deriving (Eq, Show)

{- | Used as a setting for what algorithm to run. The quadratic algorithm and Approximate
Palindrome algorithm also have functionality for including gaps and errors, therefore this
is given as an extra setting.
-}
data Algorithm
    = AlgLinear
    | AlgQuadratic {algGapSize :: Int, algMaxErrors :: Int}
    | AlgApproximate {algGapSize :: Int, algMaxErrors :: Int}
    deriving (Eq, Show)

{- | This method returns whether uneven palindromes are impossible to exist based on the
query settings.
-}
onlyEvenPals :: Variant -> Algorithm -> Bool
onlyEvenPals VarDNA (AlgQuadratic gapSize _) = even gapSize
onlyEvenPals VarDNA AlgLinear = True
onlyEvenPals _ _ = False

{- | This function combines three phases based on the settings and input given: The
pre-processing phase, the algorithm phase and the post-processing phase. It finds and
returns a list of ranges of every found palindrome in the input.
-}
findPalindromeRanges
    :: Variant -> Algorithm -> U.Vector Char -> [Range]
findPalindromeRanges variant algorithm input =
    (post . preAlg) input
  where
    {- The pre-processing phase parses the text input based on the Variant provided to a
    vector of PalEq items. -}
    preAlg :: U.Vector Char -> [Range]
    preAlg = case variant of
        VarText -> alg . filterLetters
        VarPunctuation -> alg . filterLetters
        VarDNA -> alg . tryParseDNA
        VarWord -> alg . textToWords
        _ -> alg

    {- The algorithm phase runs one of the algorithms that finds the ranges, since the linear and quadratic
    find indexLists we must convert these to ranges. -}
    alg :: (PalEq b, G.Vector v b) => v b -> [Range]
    alg = case algorithm of
        AlgLinear -> indexListToRanges . linearAlgorithm (onlyEvenPals variant algorithm)
        AlgQuadratic gapSize maxErrors ->
            indexListToRanges
                . quadraticAlgorithm
                    (onlyEvenPals variant algorithm)
                    gapSize
                    maxErrors
        AlgApproximate gapSize maxErrors -> approximateAlgorithm gapSize maxErrors

    indexListToRanges :: [Int] -> [Range]
    indexListToRanges = go 0
      where
        go _ [] = []
        -- This code adds indexes for the indexedLengthToRange function to calculate the ranges
        -- This implementation is preferred over using list generators for performance reasons
        go !i (x : xs) = indexedLengthToRange (i, x) : go (i + increment) xs
        increment
            | onlyEvenPals variant algorithm = 2
            | otherwise = 1

    {- The post-processing phase changes the list of ranges so that they fit the
    requirements in the case of punctuation palindromes -}
    post :: [Range] -> [Range]
    post = case variant of
        VarPunctuation -> filterPunctuation input
        _ -> id

{- | This function finds palindromes in the input string given the settings parameters.
It does so by first converting the input to a vector of PalEq elements.
The exact datatype depends on the Variant setting. Then using the selected Algorithm it finds the palindrome ranges.
Finally the ranges are converted to palindrome objects which also contain the actual text of the ranges.
-}
findPalindromes
    :: Variant
    -- ^ The palindrome variant to search for.
    -> Algorithm
    -- ^ The algorithm to use for finding palindromes.
    -> Int
    -- ^ The minimum length of palindromes to find.
    -> String
    -- ^ The input string.
    -> [Palindrome]
    -- ^ The list of all found maximal palindromes not smaller than the minimum length.
findPalindromes variant algorithm minlen input =
    map rangeToPalindrome $ filterRanges $ findPalindromeRanges variant algorithm inputVector
  where
    rangeToPalindrome :: Range -> Palindrome
    rangeToPalindrome r =
        Palindrome
            { palRange = r
            , palText = rangeToText (indicesInOriginal r) inputVector
            , palRangeInText = indicesInOriginal r
            }

    filterRanges :: [Range] -> [Range]
    filterRanges = filter ((>= minlen) . rangeToLength)

    -- Takes a range in the pre-processed input and returns the range in the original input.
    indicesInOriginal :: Range -> Range
    indicesInOriginal range = case variant of
        VarText -> indicesInOutputText range inputLength (filterLetters' inputVector)
        VarPunctuation -> indicesInOutputText range inputLength (filterLetters' inputVector)
        VarDNA -> indicesInOutputText range inputLength (filterLetters' inputVector)
        VarPlain -> range
        VarWord -> indicesInOutputWord range inputLength (textToWordIndices inputVector)
    !inputVector = U.fromList input
    !inputLength = U.length inputVector

{- | Shows found palindromes in the input string given the setting parameters.
It does so by finding all the palindromes, then applying the OutputFilter
and finally formatting them to a string depending on the OutputFormat.
-}
findPalindromesFormatted
    :: Variant
    -- ^ The palindrome variant to search for.
    -> OutputFormat
    -- ^ The format of the string representing the found palindromes.
    -> OutputFilter
    -- ^ The filter the output goes through to select the requested output.
    -> Algorithm
    -- ^ The algorithm to use for finding palindromes.
    -> Int
    -- ^ The minimum length of palindromes to find.
    -> String
    -- ^ The input string.
    -> String
    -- ^ The output string representing the found palindromes.
findPalindromesFormatted variant outputFormat outputFilter algorithm minlen input =
    formatPalindromes outputFormat $
        filterPalindromes outputFilter $
            findPalindromes variant algorithm minlen input

-- | Filter the list of found maximal palindromes using the selected OutputFilter.
filterPalindromes :: OutputFilter -> [Palindrome] -> [Palindrome]
filterPalindromes outputFilter = case outputFilter of
    {- reverse foldl' is more efficient in memory than foldr. This is because know the
    fold can be applied as soon as the result of the algorithm is computed. With foldr we
    would first have to compute the entire list before we can apply the filter. -}
    SelectLongest -> reverse . foldl' longest []
    SelectAt n -> filter ((== n) . rangeToPalindromeCenter . palRange)
    SelectAll -> id

-- | Show the list of palindromes using the selected OutputFormat.
formatPalindromes :: OutputFormat -> [Palindrome] -> String
formatPalindromes _ [] = "No palindromes found"
formatPalindromes outputFormat pals = case outputFormat of
    FormatLength -> showLengths pals
    FormatText -> showTexts pals
    FormatRange -> showRanges pals
    FormatAllDetails -> showAll pals
