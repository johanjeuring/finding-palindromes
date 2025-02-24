module PalindromeMethods where

import qualified Data.Algorithms.Palindromes.Palindromes as P
import qualified Data.Algorithms.Palindromes.PalindromesUtils as PU
import qualified Data.ByteString as B

{- takes a flag to edit the complexity
to enable the usage of both algorithm types on the same unit tests -}
longestTextPalindrome :: PU.Flag -> B.ByteString -> String
longestTextPalindrome t =
    P.palindrome
        (Just PU.Text)
        (Just PU.Longest)
        (Just t)
        Nothing
        Nothing
        Nothing

{- code that is probably usefull later:
longestWordPalindrome :: PU.Flag -> B.ByteString -> String
longestWordPalindrome t = palindrome (Just PU.Word) (Just PU.Longest) (Just t) Nothing Nothing Nothing -}
longestWordPalindrome =
    P.palindrome
        (Just PU.Word)
        (Just PU.Longest)
        (Just PU.Linear)
        Nothing
        Nothing
        Nothing
