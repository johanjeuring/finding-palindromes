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

import Data.Algorithms.Palindromes.Options (handleOptions, options)
import System.Console.GetOpt (ArgOrder (Permute), getOpt)
import System.Environment (getArgs)

import qualified System.IO as Sys

-----------------------------------------------------------------------------
-- main
-----------------------------------------------------------------------------

handleFilesWith :: (String -> String) -> [String] -> IO ()
handleFilesWith f [] = putStr $ f undefined
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
        input <- getContents
        putStrLn (function input)

main :: IO ()
main = do
    args <- getArgs
    let (optionArgs, files, errors) = getOpt Permute options args
    if not (null errors)
        then putStrLn (concat errors)
        else
            let (function, standardInput) = handleOptions optionArgs
            in  if standardInput
                    then handleStandardInputWith function
                    else handleFilesWith function files
