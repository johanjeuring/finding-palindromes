{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import GHC.Generics (Generic)

import Data.Algorithms.Palindromes.ApproximateAlgorithm (approximateAlgorithm)
import Data.Algorithms.Palindromes.Finders (Algorithm (..), Variant (..), findPalindromes)
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..))

import qualified System.IO as Sys
import qualified System.IO.Strict as Strict

deriving instance Generic Palindrome
deriving instance NFData Palindrome

{- | The point of this build target is to have an easy way to
forcefully evaluate functions so you can analyse their result using profiling.

Currently it contains an example comparing the linear and quadratic implementation of the findPalindromes function.
The annotation {\-# SCC "linear" #-\} is used to create a cost centre within the profiling overview
with the label being linear.
-}
main :: IO ()
main = do
    as <-
        getFileContentLatin1 "benchmarking/benchmarking-files/text-files/aaaaaaaaaaaaaas.txt"

    _ <-
        {-# SCC "linear" #-}
        evaluate $ force $ findPalindromes VarText AlgLinear 0 as
    _ <-
        {-# SCC "quadratic" #-}
        evaluate $ force $ findPalindromes VarText (AlgQuadratic 0 0) 0 as
    _ <-
        {-# SCC "approximateAlgorithm" #-}
        evaluate $ force $ findPalindromes VarText (AlgApproximate 0 0) 0 as
    return ()

-- | Strictly loads the content of a file from Latin1 encoding
getFileContentLatin1 :: String -> IO String
getFileContentLatin1 fileName = do
    handle <- Sys.openFile fileName Sys.ReadMode
    Sys.hSetEncoding handle Sys.latin1
    content <- Strict.hGetContents handle
    Sys.hClose handle
    return content
