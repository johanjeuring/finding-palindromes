{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, defaultConfig, defaultMainWith, env, nf)
import Criterion.Types (Config (..))
import Data.List (isSuffixOf)
import GHC.Generics (Generic)
import System.Directory (getDirectoryContents)
import System.FilePath (takeFileName)

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromes
    , findPalindromesFormatted
    )
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..))

import qualified System.IO as Sys
import qualified System.IO.Strict as Strict

{- Next two lines allow for force evaluation of the Palindrome datatype to Normal Form
(NF) -}
deriving instance Generic Palindrome
deriving instance NFData Palindrome

-- | Benchmarks every file in the benchmarking-files folder
main =
    do
        textFiles <- getTextFiles
        let complexityBenchmarks =
                bgroup "complexity" $ map (`benchFile` benchComplexity) textFiles
        let textVariantBenchmarks =
                bgroup "text-variants" $ map (`benchFile` benchTextVariants) textFiles
        let outputBenchmarks =
                bgroup "output" $ map (`benchFile` benchOutputOptions) textFiles

        dnaFiles <- getDnaFiles
        let dnaBenchmarks = bgroup "dna" $ map (`benchFile` benchDna) dnaFiles

        defaultMainWith
            config
            [complexityBenchmarks, textVariantBenchmarks, outputBenchmarks, dnaBenchmarks]

-- | Contains the location where we want the report file to be located
config :: Config
config = defaultConfig{reportFile = Just "benchmark-report.html"}

{- | Benchmark a single file with a function that does multiple benchmarks on that
file
-}
benchFile :: String -> (String -> [Benchmark]) -> Benchmark
benchFile filePath benchmarks =
    env (getFileContentUtf8 filePath) $
        \content ->
            bgroup (takeFileName filePath) (benchmarks content)

{- | Takes a string and creates a benchmark for every complexity option of findPalindromes
on that string.
-}
benchComplexity :: String -> [Benchmark]
benchComplexity content =
    [ bench "quadratic" $
        nf
            (findPalindromes VarText (ComQuadratic 0 0) 0)
            content
    , bench "linear" $
        nf
            (findPalindromes VarText ComLinear 0)
            content
    ]

{- | Takes a string and creates a benchmark for every text based palindrome variant of
findPalindromes on that string.
-}
benchTextVariants :: String -> [Benchmark]
benchTextVariants content =
    [ bench "plain" $
        nf
            (findPalindromes VarPlain (ComQuadratic 0 0) 0)
            content
    , bench "text" $
        nf
            (findPalindromes VarText (ComQuadratic 0 0) 0)
            content
    , bench "punctuation" $
        nf
            (findPalindromes VarPunctuation (ComQuadratic 0 0) 0)
            content
    , bench "word" $
        nf
            (findPalindromes VarWord (ComQuadratic 0 0) 0)
            content
    ]

{- | Takes a string and creates a benchmark for every output option for
findPalindromesFormatted on that string.
-}
benchOutputOptions :: String -> [Benchmark]
benchOutputOptions content =
    [ bench "length" $
        nf
            (findPalindromesFormatted VarText OutLength (ComQuadratic 0 0) 0)
            content
    , bench "lengths" $
        nf
            (findPalindromesFormatted VarText OutLengths (ComQuadratic 0 0) 0)
            content
    , bench "word" $
        nf
            (findPalindromesFormatted VarText OutWord (ComQuadratic 0 0) 0)
            content
    , bench "words" $
        nf
            (findPalindromesFormatted VarText OutWords (ComQuadratic 0 0) 0)
            content
    ]

{- | Takes a string and creates a benchmark for every output option for
findPalindromesFormatted on that string.
-}
benchDna :: String -> [Benchmark]
benchDna content =
    [ bench "plain" $
        nf
            (findPalindromes VarPlain (ComQuadratic 0 0) 0)
            content
    , bench "dna" $
        nf
            (findPalindromes VarDNA (ComQuadratic 0 0) 0)
            content
    , bench "odd gapped dna" $
        nf
            (findPalindromes VarDNA (ComQuadratic 1 0) 0)
            content
    ]

getTextFiles :: IO [String]
getTextFiles = do
    files <- getDirectoryContentsWithPath "benchmarking/benchmarking-files/text-files"
    return $ filter (isSuffixOf ".txt") files

getDnaFiles :: IO [String]
getDnaFiles = do
    files <- getDirectoryContentsWithPath "benchmarking/benchmarking-files/dna-files"
    return $ filter (isSuffixOf ".txt") files

{- | Gets the file names of every file in the given directory and then prepends the
directory to the filepath
-}
getDirectoryContentsWithPath :: String -> IO [String]
getDirectoryContentsWithPath dir = do
    files <- getDirectoryContents dir
    return (map ((dir ++ "/") ++) files)

-- | Reads the content of a file in UTF-8 encoding, also sets UTF-8 as output encoding
getFileContentUtf8 :: String -> IO String
getFileContentUtf8 fileName = do
    handle <- Sys.openFile fileName Sys.ReadMode
    Sys.hSetEncoding handle Sys.utf8
    Sys.hSetEncoding Sys.stdout Sys.utf8
    content <- Strict.hGetContents handle
    Sys.hClose handle
    return content
