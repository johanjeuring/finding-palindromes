module PalindromeMethods where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (..)
    , Output (..)
    , Variant (..)
    , createReadableCombinator
    )
import Data.Algorithms.Palindromes.PalindromesUtils
    ( Couplable
    )

import qualified Data.Algorithms.Palindromes.Combinators as C

{- takes a flag to edit the complexity
to enable the usage of both algorithm types on the same unit tests -}
longestTextPalindrome :: Complexity -> String -> String
longestTextPalindrome complexity =
    createReadableCombinator VarText OutWord complexity (0, Nothing)

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
