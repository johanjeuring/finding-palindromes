module PalindromeMethods where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (..)
    , Output (..)
    , Variant (..)
    , createReadableCombinator
    )
import Data.Algorithms.Palindromes.LinearAlgorithm
import Data.Algorithms.Palindromes.PalindromesUtils
    ( Couplable
    )
import GHC.IO.Exception (IOErrorType (NoSuchThing))

import qualified Data.Algorithms.Palindromes.Combinators as C
import qualified Data.Algorithms.Palindromes.PalindromesUtils as PU
import qualified Data.Vector as V

{- takes a flag to edit the complexity
to enable the usage of both algorithm types on the same unit tests -}
longestTextPalindrome :: Complexity -> String -> String
longestTextPalindrome complexity =
    createReadableCombinator VarText OutWord complexity (0, Nothing)

{- code that is probably usefull later:
longestWordPalindrome :: PU.Flag -> B.ByteString -> String
longestWordPalindrome t = palindrome (Just PU.Word) (Just PU.Longest) (Just t) Nothing Nothing Nothing -}
longestWordPalindrome :: Complexity -> String -> String
longestWordPalindrome t =
    createReadableCombinator
        VarWord
        OutWord
        t
        (0, Nothing)

longestPunctuationPalindrome :: String -> String
longestPunctuationPalindrome =
    createReadableCombinator
        VarPunctuation
        OutWord
        ComLinear
        (0, Nothing)

longestDNAPalindrome :: Complexity -> String -> String
longestDNAPalindrome complexity =
    createReadableCombinator
        VarDNA
        OutWord
        complexity
        (0, Nothing)

extendTextPalindrome :: Complexity -> Int -> String -> String
extendTextPalindrome complexity n =
    createReadableCombinator
        VarText
        (OutWordAt n)
        complexity
        (0, Nothing)

{-
palindrome
    (Just PU.Text)
    (Just (PU.Extend n))
    (Just complexity)
    Nothing
    Nothing
    Nothing -}
