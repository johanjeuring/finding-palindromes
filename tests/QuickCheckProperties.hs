module QuickCheckProperties where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (ComQuadratic)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    , createCombinator
    )
import Data.Algorithms.Palindromes.PalindromesUtils
    ( DNA (A, C, G, T)
    , Palindrome (..)
    , dnaToChar
    )
import Data.Algorithms.Palindromes.Settings
    ( Settings (..)
    )
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, forAll)

instance Arbitrary DNA where
    arbitrary = elements [A, T, C, G]

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
        (\pal -> palText pal == reverse (palText pal))
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )
-- | Removes gap from palindrome, and unescapes, lowercases, input
preprocessingGap :: Int -> String -> String
preprocessingGap gapLength palindrome = undefined
  where
    len = length palindrome
    toRemove = if even len == even gapLength then gapLength else gapLength - 1

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
        (\pal -> palCenterIndex pal == sumTuple (palRange pal))
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

sumTuple :: (Int, Int) -> Int
sumTuple (p, q) = p + q

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
