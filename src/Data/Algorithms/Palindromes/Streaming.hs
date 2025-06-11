{-# LANGUAGE BangPatterns #-}

module Data.Algorithms.Palindromes.Streaming (findPalindromesStream, findPalindromesWithProgressBar, findPalindromesVisualised) where

import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush)

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , Variant (..)
    , findPalindromes
    )
import Data.Algorithms.Palindromes.Output (longest)
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..), getLength)

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Vector as V
import qualified System.IO as Sys

-- | Streams the result of the given settings
findPalindromesStream
    :: Variant -> Complexity -> Int -> String -> C.ConduitT () Palindrome IO ()
findPalindromesStream variant complexity minlen input =
    C.yieldMany $ findPalindromes variant complexity minlen input

{- | Returns the result of finding palindromes with the settings (The first 3 params) on the string
whilst diplaying the intermediate progress using the given function
-}
findPalindromesVisualised
    :: Variant
    -> Complexity
    -> Int
    -> Bool
    -- ^ Enable filtering to only return the longest palindromes
    -> String
    -> (Float -> IO ())
    -- ^ Function that defines how to visualise progress
    -> IO [Palindrome]
findPalindromesVisualised variant complexity minLength filterLongest input visualiseProgress =
    do
        let chunkSize = 100
        -- Used for minimum detail level for progress, only use smaller than 2 if user explicitly overrides default
        let streamMinLength = min minLength 2
        let !inputLength = length input
        visualiseProgress 0
        result <-
            C.runConduit $
                findPalindromesStream variant complexity streamMinLength input
                    C..| C.conduitVector chunkSize -- Chunk result stream
                    C..| calcVisualiseProgress inputLength visualiseProgress
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
    :: Int -> (Float -> IO ()) -> C.ConduitT (V.Vector Palindrome) (V.Vector Palindrome) IO ()
calcVisualiseProgress totalLen visualise = C.awaitForever $ \pals -> do
    let progress = fromIntegral (sum $ palRangeInText (V.last pals)) / fromIntegral totalLen
    liftIO $ visualise progress
    C.yield pals

-- | Wrapper for findPalindromesVisualised where the visualisation method is printProgressbar
findPalindromesWithProgressBar
    :: Variant
    -> Complexity
    -> Int
    -> Bool
    -> String
    -> IO [Palindrome]
findPalindromesWithProgressBar variant complexity minLength filterLongest input =
    findPalindromesVisualised
        variant
        complexity
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
