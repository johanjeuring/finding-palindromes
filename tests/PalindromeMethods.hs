module PalindromeMethods where

import Data.Algorithms.Palindromes.PalindromesUtils (Couplable)
import Data.Word (Word8)

import qualified Data.Algorithms.Palindromes.Palindromes as P
import qualified Data.Algorithms.Palindromes.PalindromesUtils as PU
import qualified Data.Vector as V
import GHC.IO.Exception (IOErrorType(NoSuchThing))

{- takes a flag to edit the complexity
to enable the usage of both algorithm types on the same unit tests -}
longestTextPalindrome :: PU.Flag -> V.Vector Char -> String
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
longestWordPalindrome :: V.Vector Char -> String
longestWordPalindrome =
    P.palindrome
        (Just PU.Word)
        (Just PU.Longest)
        (Just PU.Linear)
        Nothing
        Nothing
        Nothing

longestDNAPalindrome :: PU.Flag -> V.Vector Char -> String
longestDNAPalindrome complexity =
    P.palindrome
        (Just PU.DNA)
        (Just PU.Longest)
        (Just complexity)
        Nothing
        Nothing
        Nothing

extendTextPalindrome :: PU.Flag -> Int -> V.Vector Char -> String
extendTextPalindrome complexity n =
    P.palindrome
        (Just PU.Text)
        (Just (PU.Extend n))
        (Just complexity)
        Nothing
        Nothing
        Nothing
