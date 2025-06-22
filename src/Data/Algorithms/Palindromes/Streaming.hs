{-# LANGUAGE BangPatterns #-}

{- |
Module      :  Data.Algorithms.Palindromes.Streaming
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

This module contains functions for streaming the search for palindromes. This is used for
the progress bar.
-}
module Data.Algorithms.Palindromes.Streaming (findPalindromesStream, findPalindromesWithProgressBar, findPalindromesVisualised) where

import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush)

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , Variant (..)
    , findPalindromes
    )
import Data.Algorithms.Palindromes.Internal.Output (longest)
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..), getLength)

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Vector as V
import qualified System.IO as Sys

-- | Streams the result of the given settings
findPalindromesStream
    :: Variant -> Algorithm -> Int -> String -> C.ConduitT () Palindrome IO ()
findPalindromesStream variant algorithm minlen input =
    C.yieldMany $ findPalindromes variant algorithm minlen input

{- | Returns the result of finding palindromes with the settings (The first 3 params) on the string
whilst diplaying the intermediate progress using the given function
-}
findPalindromesVisualised
    :: Variant
    -> Algorithm
    -> Int
    -> Bool
    -- ^ Enable filtering to only return the longest palindromes
    -> String
    -> (Float -> IO ())
    -- ^ Function that defines how to visualise progress
    -> IO [Palindrome]
findPalindromesVisualised variant algorithm minLength filterLongest input visualiseProgress =
    do
        let chunkSize = 100
        -- Used for minimum detail level for progress, only use smaller than 2 if user explicitly overrides default
        let streamMinLength = min minLength 2
        let !inputLength = length input
        visualiseProgress 0
        result <-
            C.runConduit $
                findPalindromesStream variant algorithm streamMinLength input
                    C..| C.conduitVector chunkSize -- Chunk result stream
                    C..| calcVisualiseProgress inputLength algorithm visualiseProgress
                    C..| C.concat
                    C..| C.filter -- Filter to actual given filter size
                        ((minLength <=) . getLength)
                    C..| if filterLongest
                        then
                            C.foldl longest []
                        else C.sinkList
        return $ if filterLongest then reverse result else result

{- | Consumes a Vector of Palindrome and prints the progress made
based on the centre of the palindrome range compared to the input length.
Then forces the values in the Vector to ensure progress has been made and passes them on.
-}
calcVisualiseProgress
    :: Int
    -- ^ Max length of the input vector, needed to calculate percentage
    -> Algorithm
    -- ^ Required because the Approximate algorithm returns the palindromes in reverse order
    -> (Float -> IO ())
    -- ^ Function that defines how to visualise the progress
    -> C.ConduitT (V.Vector Palindrome) (V.Vector Palindrome) IO ()
calcVisualiseProgress totalLen algorithm visualise = C.awaitForever $ \pals -> do
    let rawProgress = fromIntegral (sum $ palRangeInText (V.last pals)) / fromIntegral totalLen
    let progress = case algorithm of
            AlgApproximate _ _ -> 1 - rawProgress
            _ -> rawProgress
    liftIO $ visualise progress
    C.yield pals

-- | Wrapper for findPalindromesVisualised where the visualisation method is printProgressbar
findPalindromesWithProgressBar
    :: Variant
    -> Algorithm
    -> Int
    -> Bool
    -> String
    -> IO [Palindrome]
findPalindromesWithProgressBar variant algorithm minLength filterLongest input =
    findPalindromesVisualised
        variant
        algorithm
        minLength
        filterLongest
        input
        printProgressBar

{- | Prints a progress bar into the terminal with progress between 0 and 1,
then flushes stdout to force a write
-}
printProgressBar :: Float -> IO ()
printProgressBar progress = do
    let hashes = round $ progress * 40
    let dashes = 40 - hashes
    let percent = (round $ progress * 100) :: Int
    let bar =
            "["
                ++ replicate hashes '#'
                ++ replicate dashes '-'
                ++ "] "
                ++ show percent
                ++ "%"
    putStr $ "\r" ++ bar
    hFlush Sys.stdout
