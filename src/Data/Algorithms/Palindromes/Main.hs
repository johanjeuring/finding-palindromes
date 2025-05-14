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
handleFileWith :: (String -> String) -> String -> IO ()
handleFileWith f file = do
    print $ "Reading file: " ++ file
    file' <- Sys.openFile file Sys.ReadMode
    Sys.hSetEncoding file' Sys.utf8
    Sys.hSetEncoding Sys.stdout Sys.utf8
    input <- Sys.hGetContents file'
    putStrLn (f input)
    Sys.hClose file'
    print "--------------------------------------------------------------"

{- |
    Read all files in a directory. Ignore nested folders
-}
handleDirectoryWith :: (String -> String) -> String -> IO ()
handleDirectoryWith f dir = do
    print $ "Reading directory: " ++ dir
    content <- Dir.listDirectory dir
    let paths = map (\file -> dir ++ '/' : file) content
    files <- filterM Dir.doesFileExist paths
    handlePathsWith f files

{- |
    Read multiple paths, regardless of whether it's a file or directory
-}
handlePathsWith :: (String -> String) -> [String] -> IO ()
handlePathsWith f [] = putStrLn $ f ""
handlePathsWith f (x : xs) = do
    handlePathWith f x
    handlePathsWith f xs

{- |
    Read a path, regardless of whether it's a file or directory
-}
handlePathWith :: (String -> String) -> String -> IO ()
handlePathWith f path = do
    isFile <- Dir.doesFileExist path
    if isFile then handleFileWith f path else handleDirectoryWith f path

handleStandardInputWith :: (String -> String) -> IO ()
handleStandardInputWith function =
    do
        input <- getLine
        putStrLn (function input)

main :: IO ()
main = do
    args <- getArgs
    let (optionArgs, files, errors) = getOpt Permute options args
    if not (null errors)
        then putStrLn (concat errors)
        else
            let (function, standardInput) = handleFlags optionArgs
            in  if standardInput
                    then handleStandardInputWith function
                    else handlePathsWith function files
