-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
Module      :  Data.Algorithms.Palindromes.Main
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3

Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable
-}
module Main where

import Data.Algorithms.Palindromes.Options (options)
import Data.Algorithms.Palindromes.Settings (handleFlags)
import System.Console.GetOpt (ArgOrder (Permute), getOpt)
import System.Environment (getArgs)

import qualified System.IO as Sys

-----------------------------------------------------------------------------
-- main
-----------------------------------------------------------------------------

handleFilesWith :: (String -> String) -> [String] -> IO ()
handleFilesWith f [] = putStr $ f ""
handleFilesWith f xs =
    let hFW filenames =
            case filenames of
                [] -> putStr ""
                (fn : fns) -> do
                    fn' <- Sys.openFile fn Sys.ReadMode
                    Sys.hSetEncoding fn' Sys.latin1
                    input <- Sys.hGetContents fn'
                    putStrLn (f input)
                    Sys.hClose fn'
                    hFW fns
    in  hFW xs

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
                    else handleFilesWith function files
