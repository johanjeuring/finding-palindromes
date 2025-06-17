{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring -}
module PalindromeMethods
    ( longestTextPalindrome
    , longestWordPalindrome
    , longestPunctuationPalindrome
    , longestDNAPalindrome
    , extendTextPalindrome
    ) where

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    )
import Data.Algorithms.Palindromes.PalEq
    ( PalEq
    )

{- takes a flag to edit the algorithm
to enable the usage of both algorithm types on the same unit tests -}
longestTextPalindrome :: Algorithm -> String -> String
longestTextPalindrome algorithm =
    findPalindromesFormatted VarText FormatText SelectLongest algorithm 0

longestWordPalindrome :: Algorithm -> String -> String
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
        AlgLinear
        0

longestDNAPalindrome :: Algorithm -> String -> String
longestDNAPalindrome algorithm =
    findPalindromesFormatted
        VarDNA
        FormatText
        SelectLongest
        algorithm
        0

extendTextPalindrome :: Algorithm -> Int -> String -> String
extendTextPalindrome algorithm n =
    findPalindromesFormatted
        VarText
        FormatText
        (SelectAt n)
        algorithm
        0
