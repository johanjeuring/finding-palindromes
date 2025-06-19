{- |
Module      :  Main
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

The starting point of the program which handles commandline/IO.
-}
module Main where

import Control.Monad (filterM)
import System.Console.GetOpt (ArgOrder (Permute), getOpt, usageInfo)
import System.Environment (getArgs)

import Data.Algorithms.Palindromes.Settings (applySettingsToFinder)
import FlagsToSettings (getSettings)
import Options (Flag (..), options)

import qualified System.Directory as Dir
import qualified System.IO as Sys

-----------------------------------------------------------------------------
-- main
-----------------------------------------------------------------------------

{- |
    Read a single file
-}
handleFileWith :: (String -> IO String) -> String -> IO ()
handleFileWith f file = do
    putStrLn $ "Reading file: " ++ file
    file' <- Sys.openFile file Sys.ReadMode
    Sys.hSetEncoding file' Sys.utf8
    Sys.hSetEncoding Sys.stdout Sys.utf8
    input <- Sys.hGetContents file'
    res <- f input
    putStrLn $ '\r' : "Palindromes:" ++ replicate 40 ' ' ++ "\n" ++ res
    Sys.hClose file'
    putStrLn "--------------------------------------------------------------"

{- |
    Read all files in a directory. Ignore nested folders
-}
handleDirectoryWith :: (String -> IO String) -> String -> IO ()
handleDirectoryWith f dir = do
    print $ "Reading directory: " ++ dir
    content <- Dir.listDirectory dir
    let paths = map (\file -> dir ++ '/' : file) content
    files <- filterM Dir.doesFileExist paths
    handlePathsWith f files

{- |
    Read multiple paths, regardless of whether it's a file or directory
-}
handlePathsWith :: (String -> IO String) -> [String] -> IO ()
handlePathsWith f [] = do
    res <- f ""
    putStrLn $ "\r" ++ res ++ replicate 40 ' '
handlePathsWith f paths = handlePathsWith' f paths
  where
    -- Helper exists to not run algorithm on empty string after processing all other files
    handlePathsWith' _ [] = return ()
    handlePathsWith' f' (x : xs) = do
        handlePathWith f' x
        handlePathsWith' f' xs

{- |
    Read a path, regardless of whether it's a file or directory
-}
handlePathWith :: (String -> IO String) -> String -> IO ()
handlePathWith f path = do
    isFile <- Dir.doesFileExist path
    if isFile then handleFileWith f path else handleDirectoryWith f path

handleStandardInputWith :: (String -> IO String) -> IO ()
handleStandardInputWith function =
    do
        putStrLn "Write your input:"
        input <- getLine
        res <- function input
        putStrLn $ '\r' : "Palindromes:" ++ replicate 40 ' ' ++ "\n" ++ res
main :: IO ()
main = do
    args <- getArgs
    let (optionArgs, files, errors) = getOpt Permute options args
    if not (null errors)
        then putStrLn (concat errors)
        else
            let (function, standardInput) = handleFlags optionArgs (not $ null files)
            in  if standardInput
                    then handleStandardInputWith function
                    else handlePathsWith function files
  where
    {- Based on input flags, gets a tuple with a function that directly encapsulates
    everything from the input string to the output string. Also encodes whether input string
    is from a file or standard input. -}
    handleFlags
        :: [Flag]
        -> Bool
        -> ( String -> IO String -- function from input to output
           , Bool -- if input is standard input
           )
    handleFlags flags hasFiles =
        ( if Help `elem` flags || (null flags && not hasFiles)
            then const $ return (usageInfo headerHelpMessage options)
            else applySettingsToFinder progressDisabled (getSettings flags)
        , StandardInput `elem` flags
        )
      where
        progressDisabled = ProgressDisabled `elem` flags

    -- The header of the help message.
    headerHelpMessage :: String
    headerHelpMessage =
        "*********************\n"
            ++ "* Palindrome Finder *\n"
            ++ "* version 0.5       *\n"
            ++ "*********************\n"
            ++ "Usage: \n"
            ++ "Either give the path to a file or directory or use the flag -i for manual input in the terminal. "
            ++ "The following flags can be used to change settings."
