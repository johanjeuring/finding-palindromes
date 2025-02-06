-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithms.Palindromes.Main
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Main where

import System.Environment (getArgs)
import System.Console.GetOpt 
import System.IO

import qualified Data.ByteString as B

import Data.Algorithms.Palindromes.Options

-----------------------------------------------------------------------------
-- main
-----------------------------------------------------------------------------

handleFilesWith :: (B.ByteString -> String) -> [String] -> IO ()
handleFilesWith f [] = putStr $ f undefined 
handleFilesWith f xs = 
  let hFW filenames = 
        case filenames of
          []        ->  putStr ""
          (fn:fns)  ->  do fn' <- openFile fn ReadMode
                           hSetEncoding fn' latin1 
                           input <- B.hGetContents fn' 
                           putStrLn (f input)
                           hClose fn'
                           hFW fns
  in hFW xs       

handleStandardInputWith :: (B.ByteString -> String) -> IO ()
handleStandardInputWith function = 
  do input <- B.getContents
     putStrLn (function input) 

main :: IO ()
main = do args <- getArgs
          let (optionArgs,files,errors) = getOpt Permute options args
          if not (null errors) 
            then putStrLn (concat errors) 
            else let (function,standardInput) = handleOptions optionArgs
                 in  if standardInput 
                     then handleStandardInputWith function 
                     else handleFilesWith function files 

