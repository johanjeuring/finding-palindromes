-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- \|
-- Module      :  Data.Algorithms.Palindromes.Combinators
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
module Data.Algorithms.Palindromes.Combinators
    ( createCombinator
    , createPartialCombinator
    , createReadableCombinator
    , Variant (..)
    , OutputFormat (..)
    , Complexity (..)
    , LengthMod
    , PartialCombinator
    , ReadableCombinator
    , Combinator
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

data Variant
    = -- | Convert the text to DNA, then match A with T and G with C.
      VarDNA
    | -- | Find text palindromes, then shrink the palindromes so that all palindromes are surrounded by punctuation.
      VarPunctuation
    | -- | Ignore all characters that are not letters.
      VarText
    | -- | Find palindromes in the text exactly as it was given.
      VarPlain
    | -- | Compare words instead of individual characters to look for palindromes.
      VarWord
data OutputFormat
    = -- | The length of the longest palindrome
      OutLength
    | -- | The longest palindrome as text
      OutWord
    | -- | The lengths of all maximal palindromes around each center
      OutLengths
    | -- | All maximal palindromes around each center as text
      OutWords
    | -- | The length of the palindrome at a certain center index
      OutLengthAt Int
    | -- | The palindrome at a certain center index as text
      OutWordAt Int
data Complexity
    = ComLinear
    | ComQuadratic {gapSize :: Int, maxError :: Int}

-- | The minimum and maximum length of the palindromes you want to find.
type LengthMod = (Int, Maybe Int)

type PartialCombinator = String -> [Int]
type Combinator = String -> [Palindrome]
type ReadableCombinator = String -> String

{- |
    A partial combinator is a function consisting of three phases:
    The pre-processing phase, the algorithm phase and the post-processing phase.
    It returns a list of integers, which corresponds to the lengths of the maximal palindromes around each center.
-}
createPartialCombinator
    :: Variant -> Complexity -> LengthMod -> PartialCombinator
createPartialCombinator variant complexity (minlength, maxlength') input = (post . preAlg) input
  where
    -- The pre-processing phase parses the text input based on the Variant provided to a vector of PalEq items.
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

    -- The algorithm phase runs one of the algorithms that finds the maximal palindromes around all centers.
    alg :: (PalEq b) => V.Vector b -> [Int]
    alg = case complexity of
        ComLinear -> linearAlgorithm isAntiReflexive
        _ -> quadraticAlgorithm isAntiReflexive (gapSize complexity) (maxError complexity)
    -- We distinguish this case since for anti reflexive coupleables (only DNA currently) we only need to run on odd indices saving time and space.
    isAntiReflexive :: Bool
    isAntiReflexive = case variant of
        VarDNA -> True
        _ -> False

    {- The post-processing phase changes the list of centers so that all lengths fit the requirements,
       such as shrinking the sizes so that the palindrome is surrounded by punctuation,
       applying a minimum length and applying a maximum length. -}
    post :: [Int] -> [Int]
    post = case variant of
        VarPunctuation -> filterMin minlength . filterMax maxlength . filterPunctuation input
        _ -> filterMin minlength . filterMax maxlength
    maxlength :: Int
    maxlength
        | isNothing maxlength' = length input
        | otherwise = fromJust maxlength'

{- |
    A combinator is a function consisting of 4 phases:
    The pre-processing phase, the algorithm phase, the post-processing phase and the parsing phase.
    The first three phases are taken from the partial combinator.
    The final phase parses the [Int] to a [Palindrome], as defined in PalindromesUtils.hs.
-}
createCombinator :: Variant -> Complexity -> LengthMod -> Combinator
createCombinator variant complexity lengthmod input = map lengthToPalindrome lengths
  where
    lengthToPalindrome :: (Int, Int) -> Palindrome
    lengthToPalindrome (index, len) =
        Palindrome
            { palCenterIndex = index
            , palLength = len
            , palText = indicesToText (indicesInOriginal (index, len)) input
            , palRange = indicesInOriginal (index, len)
            }

    -- A list of tuples containing the center index and the length of the maximal palindrome.
    lengths :: [(Int, Int)]
    lengths = zip [0 ..] $ createPartialCombinator variant complexity lengthmod input

    -- A function that converts a (center index, length) pair to a (start index, end index) pair
    indicesInOriginal :: (Int, Int) -> (Int, Int)
    indicesInOriginal il@(i, l) = case variant of
        VarText -> indicesInOutputText range input
        VarPunctuation -> indicesInOutputText range input
        VarDNA -> indicesInOutputText dnaRange input
        VarPlain -> range
        VarWord -> indicesInOutputWord range input
      where
        range :: (Int, Int)
        range = indexedLengthToRange il
        dnaRange :: (Int, Int)
        dnaRange = (i - (l `div` 2), i + (l `div` 2))

{- |
    A readable combinator is a function that consists of 5 phases:
    The pre-processing, the algorithm phase, the post processing phase, the parsing phase and the output phase.
    The first 3 are handled by the partial combinator and the first 4 are handled by the combinator.
    The final phase takes the OutputFormat flag into account and returns a String that can be printed.
-}
createReadableCombinator
    :: Variant -> OutputFormat -> Complexity -> LengthMod -> ReadableCombinator
createReadableCombinator variant outputFormat complexity lengthmod input = text
  where
    result :: [Palindrome]
    result = createCombinator variant complexity lengthmod input
    lengths :: [Int]
    lengths = createPartialCombinator variant complexity lengthmod input
    text :: String
    text = case outputFormat of
        OutLength -> longestLength lengths
        OutWord -> longestWord result
        OutLengths -> allLengths lengths
        OutWords -> allWords result
        OutLengthAt x -> lengthAt x lengths
        OutWordAt x -> wordAt x result