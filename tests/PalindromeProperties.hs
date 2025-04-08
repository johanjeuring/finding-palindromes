{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}
module PalindromeProperties where

import Data.Char (isAlpha, readLitChar, toLower)
import PalindromeMethods (longestTextPalindrome)
import Test.QuickCheck (Gen, Property, arbitrary, forAll)

import qualified Data.Algorithms.Palindromes.Finders as C
import qualified Data.Algorithms.Palindromes.LinearAlgorithm as LA

longestPalindromesQ :: String -> [Int]
longestPalindromesQ input =
    let (afirst, alast) = (0, length input - 1)
        positions = [0 .. 2 * (alast - afirst + 1)]
    in  map (lengthPalindromeAround input) positions

lengthPalindromeAround :: String -> Int -> Int
lengthPalindromeAround input position
    | even position =
        extendPalindromeAround (afirst + pos - 1) (afirst + pos)
    | odd position =
        extendPalindromeAround (afirst + pos - 1) (afirst + pos + 1)
  where
    pos = div position 2
    (afirst, alast) = (0, length input - 1)
    extendPalindromeAround start end =
        if start < 0
            || end > alast - afirst
            || input !! start /= input !! end
            then end - start - 1
            else extendPalindromeAround (start - 1) (end + 1)

propTextPalindrome :: Property
propTextPalindrome =
    forAll (arbitrary :: Gen [Char]) $
        \l ->
            let ltp = longestTextPalindrome C.ComLinear l
                ltp' = map toLower (filter isAlpha (unescape ltp))
            in  ltp' == reverse ltp'

unescape :: String -> String
unescape [] = []
unescape cs = case readLitChar cs of
    (c, rest) : xs -> c : unescape rest
    [] -> []