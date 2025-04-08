{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}
module PalindromeMethods where

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    )
import Data.Algorithms.Palindromes.PalEq
    ( PalEq
    )

import qualified Data.Algorithms.Palindromes.Finders as C

{- takes a flag to edit the complexity
to enable the usage of both algorithm types on the same unit tests -}
longestTextPalindrome :: Complexity -> String -> String
longestTextPalindrome complexity =
    findPalindromesFormatted VarText OutWord complexity (0, Nothing)

longestWordPalindrome :: Complexity -> String -> String
longestWordPalindrome t =
    findPalindromesFormatted
        VarWord
        OutWord
        t
        (0, Nothing)

longestPunctuationPalindrome :: String -> String
longestPunctuationPalindrome =
    findPalindromesFormatted
        VarPunctuation
        OutWord
        ComLinear
        (0, Nothing)

longestDNAPalindrome :: Complexity -> String -> String
longestDNAPalindrome complexity =
    findPalindromesFormatted
        VarDNA
        OutWord
        complexity
        (0, Nothing)

extendTextPalindrome :: Complexity -> Int -> String -> String
extendTextPalindrome complexity n =
    findPalindromesFormatted
        VarText
        (OutWordAt n)
        complexity
        (0, Nothing)
