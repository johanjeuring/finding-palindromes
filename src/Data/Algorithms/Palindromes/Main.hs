{- |
Module      :  Data.Algorithms.Palindromes.Main
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

Handles commandline.
-}
module Main where

import Control.Monad (filterM)
import System.Console.GetOpt (ArgOrder (Permute), getOpt)
import System.Environment (getArgs)

import Data.Algorithms.Palindromes.Options (options)
import Data.Algorithms.Palindromes.Settings (handleFlags)

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
