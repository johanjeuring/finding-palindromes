{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}
module PalindromeMethods
    ( longestTextPalindrome
    , longestWordPalindrome
    , longestPunctuationPalindrome
    , longestDNAPalindrome
    , extendTextPalindrome
    ) where

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    )
import Data.Algorithms.Palindromes.PalEq
    ( PalEq
    )

{- takes a flag to edit the complexity
to enable the usage of both algorithm types on the same unit tests -}
longestTextPalindrome :: Complexity -> String -> String
longestTextPalindrome complexity =
    findPalindromesFormatted VarText FormatText SelectLongest complexity 0

longestWordPalindrome :: Complexity -> String -> String
longestWordPalindrome t =
    findPalindromesFormatted
        VarWord
        FormatText
        SelectLongest
        t
        0

longestPunctuationPalindrome :: String -> String
longestPunctuationPalindrome =
    findPalindromesFormatted
        VarPunctuation
        FormatText
        SelectLongest
        ComLinear
        0

longestDNAPalindrome :: Complexity -> String -> String
longestDNAPalindrome complexity =
    findPalindromesFormatted
        VarDNA
        FormatText
        SelectLongest
        complexity
        0

extendTextPalindrome :: Complexity -> Int -> String -> String
extendTextPalindrome complexity n =
    findPalindromesFormatted
        VarText
        FormatText
        (SelectAt n)
        complexity
        0
