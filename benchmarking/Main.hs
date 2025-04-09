import Criterion.Main
import Data.Algorithms.Palindromes.Finders
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents)

import qualified System.IO as Sys
import qualified System.IO.Strict as Strict

-- | Benchmarks every file in the benchmarking-files folder
main =
    do
        files <- getFiles
        let benchmarks = map benchFile files
        defaultMain benchmarks

{- | Creates a group benchmarks on the contents of the specified file name
| Evaluates the result to normal form to ensure that you actually preform the required calculations
-}
benchFile :: String -> Benchmark
benchFile fileName =
    env (getFileContentLatin1 ("benchmarking/benchmarking-files/" ++ fileName)) $
        \content ->
            bgroup
                fileName
                [ bench "Quadratic" $
                    nf
                        (findPalindromeLengths VarText (ComQuadratic 0 0) (0, Nothing))
                        content
                , bench "Linear" $
                    nf
                        (findPalindromeLengths VarText ComLinear (0, Nothing))
                        content
                ]

-- | Get the file names of every file in the benchmarking-files directory
getFiles :: IO [String]
getFiles = do
    files <- getDirectoryContents "benchmarking/benchmarking-files"
    return $ filter (isSuffixOf ".txt") files

-- | Reads the content of a file in latin1 encoding
getFileContentLatin1 :: String -> IO String
getFileContentLatin1 fileName = do
    handle <- Sys.openFile fileName Sys.ReadMode
    Sys.hSetEncoding handle Sys.latin1
    content <- Strict.hGetContents handle
    Sys.hClose handle
    return content
