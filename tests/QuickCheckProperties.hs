module QuickCheckProperties where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (ComLinear, ComQuadratic)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    , createCombinator
    )
import Data.Algorithms.Palindromes.PalindromesUtils
    ( Couplable (..)
    , DNA (A, C, G, T)
    , Palindrome (..)
    , dnaToChar
    )
import Data.Algorithms.Palindromes.Settings
    ( Settings (..)
    )
import Data.Char (isAlpha, readLitChar, toLower)
import PalindromeMethods (longestTextPalindrome)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, forAll)

import qualified Data.Algorithms.Palindromes.Combinators as C

instance Arbitrary DNA where
    arbitrary = elements [A, T, C, G]

-- General properties -------------------------------------------------

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

-- Property 1 ---------------------------------------------------------

propValidPalindromeRangeAndTextPlain :: Property
propValidPalindromeRangeAndTextPlain = propValidPalindromeRangeAndText VarPlain
propValidPalindromeRangeAndTextText :: Property
propValidPalindromeRangeAndTextText = propValidPalindromeRangeAndText VarText
propValidPalindromeRangeAndTextWord :: Property
propValidPalindromeRangeAndTextWord = propValidPalindromeRangeAndText VarWord

-- | Test if all the generated Palindrome objects have a valid text related to the range property
propValidPalindromeRangeAndText :: Variant -> Property
propValidPalindromeRangeAndText variant = forAll (arbitrary :: Gen [Char]) $ \originalString ->
    all
        (`checkPalRangeAndText` originalString)
        (createCombinator variant (ComQuadratic 0 0) (0, Nothing) originalString)

propValidPalindromeRangeAndTextDNA :: Property
propValidPalindromeRangeAndTextDNA = forAll (arbitrary :: Gen [DNA]) $ \dna ->
    all
        (`checkPalRangeAndText` map dnaToChar dna)
        (createCombinator VarDNA (ComQuadratic 0 0) (0, Nothing) (map dnaToChar dna))

{- | Check that taking the substring of the original text described by the start and end of the palRange
property is equal to the palText property
-}
checkPalRangeAndText :: Palindrome -> String -> Bool
checkPalRangeAndText (Palindrome _ _ "" (x, y)) _ = x == y
checkPalRangeAndText (Palindrome _ _ palText (start, end)) originalString = palText == substringFromRange
  where
    substringFromRange = take (end - start) (drop start originalString)

-- Property 2 ---------------------------------------------------------

-- | Check that a found palindrome is actually a palindrome
propValidPalindromeReverse :: Settings -> Property
propValidPalindromeReverse settings = forAll (arbitrary :: Gen [Char]) $ \originalString ->
    all
        ( \pal ->
            let palindrome = getPalindromeGap settings (palText pal)
            in  palindrome == reverse palindrome
        )        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

getPalindromeGap :: Settings -> String -> String
getPalindromeGap settings palindrome = case complexity settings of
    ComQuadratic gap err -> preprocessingGap gap palindrome
    ComLinear -> preprocessingGap 0 palindrome

-- | Removes gap from palindrome, and unescapes, lowercases, input
preprocessingGap :: Int -> String -> String
preprocessingGap gapLength palindrome = map toLower (filter isAlpha (unescape palindromeNoGap))
  where
    palindromeNoGap = take start palindrome ++ drop end palindrome
    start = (length palindrome - toRemove) `div` 2
    end = start + toRemove
    toRemove =
        if even (length palindrome) == even gapLength || gapLength == 0
            then gapLength
            else gapLength - 1

-- Property 3 ---------------------------------------------------------

propPalLength :: Settings -> Property
propPalLength settings = forAll (arbitrary :: Gen [Char]) $ \originalString ->
    all
        (\pal -> length (palText pal) == palLength pal)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- Property 4 ---------------------------------------------------------

propPalCenterIndex :: Settings -> Property
propPalCenterIndex settings = forAll (arbitrary :: Gen [Char]) $ \originalString ->
    all
        (\pal -> let (start, end) = palRange pal in palCenterIndex pal == start + end)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- Property 5 ---------------------------------------------------------

propPalRange :: Settings -> Property
propPalRange settings = forAll (arbitrary :: Gen [Char]) $ \originalString ->
    all
        (\pal -> fst (palRange pal) >= 0 && snd (palRange pal) <= palLength pal)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )
