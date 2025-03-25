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
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , Property
    , arbitrary
    , choose
    , elements
    , forAll
    , listOf
    , suchThat
    )
import Text.ParserCombinators.ReadP (char)

import qualified Data.Algorithms.Palindromes.Combinators as C
import qualified QuickCheckSettings as QS

instance Arbitrary DNA where
    arbitrary = elements [A, T, C, G]

propertyList :: [Property]
propertyList =
    propTextPalindrome
        : map propValidPalindromeRangeAndText QS.settingsList
        ++ map propValidPalindromeReverse QS.settingsList
        ++ map propValidPalLength QS.settingsList
        -- ++ map propValidCenterIndex QS.settingsList
        ++ map propValidPalRange QS.settingsList
        ++ map propAllowedPalLength QS.settingsList

charGenerator :: Gen Char
charGenerator = choose (' ', '~') `suchThat` (`notElem` ['\\', '"'])

stringGenerator :: Gen [Char]
stringGenerator = listOf charGenerator

dnaGenerator :: Gen [DNA]
dnaGenerator = arbitrary :: Gen [DNA]

cleanOriginalString :: String -> String
cleanOriginalString string = map toLower (filter isAlpha (unescape string))

-- General properties -------------------------------------------------

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

-- | Test if all the generated Palindrome objects have a valid text related to the range property
propValidPalindromeRangeAndText :: Settings -> Property
propValidPalindromeRangeAndText settings = forAll stringGenerator $ \originalString ->
    all
        (`checkPalRangeAndText` originalString)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

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
propValidPalindromeReverse settings = forAll stringGenerator $ \originalString ->
    all
        (isPalindromeGapsErrors settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Checks if the string is a palindrome, taking gaps and errors into account
isPalindromeGapsErrors :: Settings -> Palindrome -> Bool
isPalindromeGapsErrors settings pal = mismatches <= errors
  where
    pal' = case variant settings of
        VarWord -> removeGap gapLength (words (cleanOriginalString (palText pal)))
        _ -> removeGap gapLength (map (: []) (cleanOriginalString (palText pal)))
    (gapLength, errors) = case complexity settings of
        ComQuadratic gap err -> (gap, err)
        ComLinear -> (0, 0)
    mismatches =
        length
            [() | i <- [0 .. (length pal' `div` 2) - 1], pal' !! i /= pal' !! (length pal' - 1 - i)]

-- | Removes gap from palindrome
removeGap :: Int -> [a] -> [a]
removeGap gapLength palindrome = take start palindrome ++ drop end palindrome
  where
    start = (length palindrome - toRemove) `div` 2
    end = start + toRemove
    toRemove =
        if even (length palindrome) == even gapLength || gapLength == 0
            then gapLength
            else gapLength - 1

-- Property 3 ---------------------------------------------------------

propValidPalLength :: Settings -> Property
propValidPalLength settings = forAll stringGenerator $ \originalString ->
    all
        (validPalLength settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

validPalLength :: Settings -> Palindrome -> Bool
validPalLength settings pal = case variant settings of
    VarWord -> length (words (cleanOriginalString $ palText pal)) == palLength pal
    _ -> length (cleanOriginalString $ palText pal) == palLength pal

-- Property 4 ---------------------------------------------------------

propValidCenterIndex :: Settings -> Property
propValidCenterIndex settings = forAll (arbitrary :: Gen [Char]) $ \originalString ->
    all
        (\pal -> let (start, end) = palRange pal in palCenterIndex pal == start + end)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- Property 5 ---------------------------------------------------------
propValidPalRange :: Settings -> Property
propValidPalRange settings = forAll stringGenerator $ \originalString ->
    all
        (\pal -> fst (palRange pal) >= 0 && snd (palRange pal) <= length originalString)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- Property 6 ---------------------------------------------------------

-- Property 7 ---------------------------------------------------------

-- Property 8 ---------------------------------------------------------
propAllowedPalLength :: Settings -> Property
propAllowedPalLength settings = forAll stringGenerator $ \originalString ->
    all
        (isAllowedPalLength settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

isAllowedPalLength :: Settings -> Palindrome -> Bool
isAllowedPalLength settings pal = case lengthMod settings of
    (l, Just u) -> palLength pal >= l && palLength pal <= u
    (l, Nothing) -> palLength pal >= l