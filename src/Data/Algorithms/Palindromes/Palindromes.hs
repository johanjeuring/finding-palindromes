-- test strict integers
-- >let input = Data.ByteString.pack (map Data.ByteString.Internal.c2w "yabadabadoo")
-----------------------------------------------------------------------------
--
-- Module      :  Data.Algorithms.Palindromes.Palindromes
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Algorithms.Palindromes.Palindromes
    ( palindrome
    , palindromesAroundCentres
    ) where

import Data.Algorithms.Palindromes.LinearAlgorithm
    ( extendPalindromeS
    , extendTailWord
    )
import Data.Algorithms.Palindromes.PalindromesUtils
    ( Flag (..)
    , showPalindrome
    , showPalindromeDNA
    , showTextPalindrome
    , toDNA
    , vecToArray
    )
import Data.Algorithms.Palindromes.QuadraticAlgorithm
    ( gappedApproximatePalindromesAroundCentres
    )
import Data.Array (Array)
import Data.Char (isAlpha, toLower)
import Data.List (intercalate, maximumBy)

import qualified Data.Bifunctor as BiFunc
import qualified Data.Vector as V

-----------------------------------------------------------------------------
-- palindrome dispatches to the desired variant of the palindrome finding
-- algorithm. It captures all the variablity, in input format, output format,
-- and length restrictions. Variability has been `pushed down' into the code
-- as much as possible, using extra arguments whenever needed, for example
-- for word palindromes (which have not been implemented correctly at the
-- moment: I do get the longest word palindromes, but shorter ones may
-- actually not be word palindromes).
-----------------------------------------------------------------------------

-- | palindrome captures all possible variants of finding palindromes.
palindrome
    :: Maybe Flag
    -> Maybe Flag
    -> Maybe Flag
    -> Maybe Flag
    -> Maybe Flag
    -> Maybe Flag
    -> V.Vector Char
    -- ^ For now, we use Word8 until we can abstract the whole datatype away or decide to use another datatype
    -> String
palindrome palindromeVariant outputFormat algorithmComplexity lengthModifier gap nrOfErrors input =
    let (predicate, correctLengthInput) = case lengthModifier of
            Just (LengthAtLeast m) -> ((m <=), m >= 0)
            Just (LengthAtMost m) -> ((<= m), m >= 0)
            Just (LengthExact m) -> (\l -> m <= l && (odd l == odd m), m >= 0)
            Just (LengthBetween m n) -> (\pl -> pl >= m && pl <= n, m >= 0 && n >= 0)
            _ -> (const True, True)

        post = case lengthModifier of
            Just (LengthExact m) -> const m
            _ -> id

        correctGapInput = case gap of
            Just (Gap gapSize) -> gapSize >= 0
            _ -> True

        correctErrorInput = case gap of
            Just (NrOfErrors n) -> n >= 0
            _ -> True

        textinput = V.map toLower (V.filter isAlpha input)

        positionTextInput = vecToArray $ V.findIndices isAlpha input

        input' = case palindromeVariant of
            Just Text -> textinput
            Just Word -> textinput
            Just DNA -> textinput
            _ -> input

        show' = case palindromeVariant of
            Just Text -> showTextPalindrome input positionTextInput
            Just Word -> showTextPalindrome input positionTextInput
            Just DNA -> showPalindromeDNA textinput
            _ -> showPalindrome input

        outputf = case outputFormat of
            Just LengthLongest -> show . maximum . map post . filter predicate
            Just Maximal ->
                intercalate "\n"
                    . map (show' . BiFunc.first post)
                    . filter (predicate . fst)
                    . flip zip [0 ..]
            Just LengthMaximal -> show . map post . filter predicate
            Just (Extend n) ->
                show'
                    . flip (!!) n
                    . flip zip [0 ..]
            _ ->
                show'
                    . maximumBy (\(l, _) (l', _) -> compare l l')
                    . map (BiFunc.first post)
                    . filter (predicate . fst)
                    . flip zip [0 ..]
    in  if not correctLengthInput || not correctGapInput || not correctErrorInput
            then error "Invalid input"
            else
                outputf $
                    palindromesAroundCentres
                        palindromeVariant
                        algorithmComplexity
                        gap
                        nrOfErrors
                        input
                        input'
                        positionTextInput

{-
-- The following code is replaced by the equivalent code using a more efficient
-- data structure. It is kept here because this is most probably easier to understand,
-- and it is the code explained on the blog.

-----------------------------------------------------------------------------
-- palindromesAroundCentres
--
-- The function that implements the palindrome finding algorithm.
-- Used in all the above interface functions.
-----------------------------------------------------------------------------

-- | palindromesAroundCentres is the central function of the module. It returns
--   the list of lenghths of the longest palindrome around each position in a
--   string.
palindromesAroundCentres        :: B.ByteString -> [Int]
palindromesAroundCentres input  =  reverse $ extendPalindrome input 0 0 []

extendPalindrome :: B.ByteString -> Int -> Int -> [Int] -> [Int]
extendPalindrome input rightmost currentPalindrome currentMaximalPalindromes
  | rightmost > last
      -- reached the end of the array
      =  finalPalindromes currentPalindrome currentMaximalPalindromes (currentPalindrome:currentMaximalPalindromes)
  | rightmost-currentPalindrome == first ||
    not (B.index input rightmost == B.index input (rightmost-currentPalindrome-1))
      -- the current palindrome extends to the start of the array,
      -- or it cannot be extended
      =  moveCenter input rightmost (currentPalindrome:currentMaximalPalindromes) currentMaximalPalindromes currentPalindrome
  | otherwise
      -- the current palindrome can be extended
      =  extendPalindrome input (rightmost+1) (currentPalindrome+2) currentMaximalPalindromes
  where  first = 0
         last  = B.length input - 1

moveCenter :: B.ByteString -> Int -> [Int] -> [Int] -> Int -> [Int]
moveCenter input rightmost currentMaximalPalindromes previousMaximalPalindromes nrOfCenters
  | nrOfCenters == 0
      -- the last centre is on the last element: try to extend the tail of length 1
      =  extendPalindrome input (rightmost+1) 1 currentMaximalPalindromes
  | nrOfCenters-1 == head previousMaximalPalindromes
      -- the previous element in the centre list reaches exactly to the end of the last
      -- tail palindrome use the mirror property of palindromes to find the longest tail palindrome
      =  extendPalindrome input rightmost (head previousMaximalPalindromes) currentMaximalPalindromes
  | otherwise
      -- move the centres one step add the length of the longest palindrome to the centres
      =  moveCenter input rightmost (min (head previousMaximalPalindromes) (nrOfCenters-1):currentMaximalPalindromes) (tail previousMaximalPalindromes) (nrOfCenters-1)

finalPalindromes :: Int -> [Int] -> [Int] -> [Int]
finalPalindromes nrOfCenters previousMaximalPalindromes currentMaximalPalindromes
  | nrOfCenters == 0
      =  currentMaximalPalindromes
  | nrOfCenters > 0
      =  finalPalindromes (nrOfCenters-1) (tail previousMaximalPalindromes) (min (head previousMaximalPalindromes) (nrOfCenters-1):currentMaximalPalindromes)
  | otherwise
      =  error "finalCentres: input < 0"

-}
-----------------------------------------------------------------------------
-- palindromesAroundCentresS
--
-- The function that implements the palindrome finding algorithm.
-- Used in all the above interface functions.
--
-- I use the Seq datatype to pass on the maximal palindromes that are used for
-- finding the maximal palindromes to the right of the center of the current
-- longest tail paindrome.
-----------------------------------------------------------------------------

{- | palindromesAroundCentres is the central function of the module. It returns
  the list of lenghths of the longest palindrome around each position in a
  string.
-}
palindromesAroundCentres
    :: Maybe Flag
    -> Maybe Flag
    -> Maybe Flag
    -> Maybe Flag
    -> V.Vector Char
    -> V.Vector Char
    -> Array Int Int
    -> [Int]
palindromesAroundCentres
    palindromeVariant
    algorithmComplexity
    gap
    nrOfErrors
    input
    input'
    positionTextInput =
        case (algorithmComplexity, gap, nrOfErrors) of
            (Just Linear, Nothing, Nothing) -> case palindromeVariant of
                Just DNA -> reverse $ extendPalindromeS 2 0 input' [] 0 0
                Just Word ->
                    reverse $ map (head . snd) $ extendTailWord input input' positionTextInput [] 0 (0, [0])
                _ -> reverse $ extendPalindromeS 1 1 input' [] 0 0
            (Just Linear, _, _) ->
                error
                    "palindromesAroundCentres: cannot calculate approximate or gapped palindromes using the linear-time algorithm"
            (_, gapSize, errorCount) ->
                let gapSize' = case gapSize of
                        Just (Gap gapSize'') -> gapSize''
                        _ -> 0
                    errorCount' = case errorCount of
                        Just (NrOfErrors errorCount'') -> errorCount''
                        _ -> 0
                    result = case palindromeVariant of
                        Just DNA ->
                            gappedApproximatePalindromesAroundCentres
                                palindromeVariant
                                (toDNA input')
                                gapSize'
                                errorCount'
                        _ ->
                            gappedApproximatePalindromesAroundCentres
                                palindromeVariant
                                input'
                                gapSize'
                                errorCount'
                in  result
