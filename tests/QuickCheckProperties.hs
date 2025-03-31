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
    , charToDNA
    , dnaToChar
    )
import Data.Algorithms.Palindromes.Settings
    ( Settings (..)
    )
import Data.Char (isAlpha, readLitChar, toLower)
import PalindromeMethods (longestTextPalindrome)
import QuickCheckSettings (settingsListChar, settingsListDNA)
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

import qualified Data.Algorithms.Palindromes.Combinators as C

instance Arbitrary DNA where
    arbitrary = elements [A, T, C, G]

propertyList :: [Property]
propertyList =
    map propValidBoundariesChar settingsListChar
        ++ map propValidBoundariesDNA settingsListDNA

propertyListn :: [Property]
propertyListn =
    -- Property 1
    map propValidPalindromeRangeAndTextChar settingsListChar
        ++ map propValidPalindromeRangeAndTextDNA settingsListDNA
        -- Property 2
        ++ map propValidPalindromeReverseChar settingsListChar
        ++ map propValidPalindromeReverseDNA settingsListDNA
        -- Property 3
        ++ map propValidPalLengthChar settingsListChar
        ++ map propValidPalLengthDNA settingsListDNA
        -- Property 4
        ++ map propValidBoundariesChar settingsListChar
        ++ map propValidBoundariesDNA settingsListDNA
        -- Property 5
        ++ map propValidPalRangeChar settingsListChar
        ++ map propValidPalRangeDNA settingsListChar
        -- Property 6
        ++ map propAllowedPalLengthChar settingsListChar
        ++ map propAllowedPalLengthDNA settingsListDNA

charGenerator :: Gen Char
charGenerator = choose (' ', '~') `suchThat` (`notElem` ['\\', '"'])

stringGenerator :: Gen [Char]
stringGenerator = listOf charGenerator

dnaGenerator :: Gen [DNA]
dnaGenerator = arbitrary :: Gen [DNA]

cleanOriginalString :: String -> String
cleanOriginalString string = map toLower (filter isAlpha string)

-- Property 1 ---------------------------------------------------------

-- | Test if all the generated Palindrome objects have a valid text related to the range property
propValidPalindromeRangeAndTextChar :: Settings -> Property
propValidPalindromeRangeAndTextChar settings = forAll stringGenerator $ \originalString ->
    all
        (`checkPalRangeAndText` originalString)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

propValidPalindromeRangeAndTextDNA :: Settings -> Property
propValidPalindromeRangeAndTextDNA settings = forAll dnaGenerator $ \dnaSeq ->
    all
        (`checkPalRangeAndText` map dnaToChar dnaSeq)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
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
propValidPalindromeReverseChar :: Settings -> Property
propValidPalindromeReverseChar settings = forAll stringGenerator $ \originalString ->
    all
        (isPalindromeGapsErrorsChar settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

propValidPalindromeReverseDNA :: Settings -> Property
propValidPalindromeReverseDNA settings = forAll dnaGenerator $ \dnaSeq ->
    all
        (isPalindromeGapsErrorsDNA settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

-- | Checks if the string is a palindrome, taking gaps and errors into account
isPalindromeGapsErrorsChar :: Settings -> Palindrome -> Bool
isPalindromeGapsErrorsChar settings pal = mismatches <= errors
  where
    pal' = case variant settings of
        VarWord -> removeGap gapLength (words (cleanOriginalString (palText pal)))
        VarPlain -> removeGap gapLength (map (: []) (palText pal))
        _ -> removeGap gapLength (map (: []) (cleanOriginalString (palText pal)))
    (gapLength, errors) = case complexity settings of
        ComQuadratic gap err -> (gap, err)
        ComLinear -> (0, 0)
    mismatches =
        length
            [ ()
            | i <- [0 .. (length pal' `div` 2) - 1]
            , not $ (pal' !! i) =:= (pal' !! (length pal' - 1 - i))
            ]

isPalindromeGapsErrorsDNA :: Settings -> Palindrome -> Bool
isPalindromeGapsErrorsDNA settings pal = mismatches <= errors
  where
    pal' = removeGap gapLength (map charToDNA (palText pal))
    (gapLength, errors) = case complexity settings of
        ComQuadratic gap err -> (gap, err)
        ComLinear -> (0, 0)
    mismatches =
        length
            [ ()
            | i <- [0 .. (length pal' `div` 2) - 1]
            , (pal' !! i) =:= (pal' !! (length pal' - 1 - i))
            ]

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

propValidPalLengthChar :: Settings -> Property
propValidPalLengthChar settings = forAll stringGenerator $ \originalString ->
    all
        (validPalLength settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

propValidPalLengthDNA :: Settings -> Property
propValidPalLengthDNA settings = forAll dnaGenerator $ \dnaSeq ->
    all
        (validPalLength settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

validPalLength :: Settings -> Palindrome -> Bool
validPalLength settings pal = case variant settings of
    VarWord -> length (words (palText pal)) == palLength pal
    VarPlain -> length (palText pal) == palLength pal
    VarDNA -> length (palText pal) == palLength pal
    _ -> length (cleanOriginalString $ palText pal) == palLength pal

-- Property 4 ---------------------------------------------------------

propValidBoundariesChar :: Settings -> Property
propValidBoundariesChar settings = forAll stringGenerator $ \originalString ->
    all
        (checkValidBoundaries settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

propValidBoundariesDNA :: Settings -> Property
propValidBoundariesDNA settings = forAll dnaGenerator $ \dnaSeq ->
    all
        (checkValidBoundaries settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

checkValidBoundaries :: Settings -> Palindrome -> Bool
checkValidBoundaries settings pal = case variant settings of
    VarPlain -> let (s, e) = palRange pal in e - s == palLength pal
    VarWord -> countWordsInRange (palRange pal) (palText pal) == palLength pal
    VarDNA -> let (s, e) = palRange pal in e - s == palLength pal
    _ ->
        let (s, e) = palRange pal in e - s - amountOfNonAlpha 0 (palText pal) == palLength pal

countWordsInRange :: (Int, Int) -> String -> Int
countWordsInRange (s, e) palString = length . words $ take (e - s) (drop s palString)

amountOfNonAlpha :: Int -> String -> Int
amountOfNonAlpha acc [] = acc
amountOfNonAlpha acc (x : xs)
    | not (isAlpha x) = amountOfNonAlpha (acc + 1) xs
    | otherwise = amountOfNonAlpha acc xs

-- Property 5 ---------------------------------------------------------
propValidPalRangeChar :: Settings -> Property
propValidPalRangeChar settings = forAll stringGenerator $ \originalString ->
    all
        (\pal -> fst (palRange pal) >= 0 && snd (palRange pal) <= length originalString)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

propValidPalRangeDNA :: Settings -> Property
propValidPalRangeDNA settings = forAll dnaGenerator $ \dnaSeq ->
    all
        (\pal -> fst (palRange pal) >= 0 && snd (palRange pal) <= length dnaSeq)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

-- Property 6 ---------------------------------------------------------
propAllowedPalLengthChar :: Settings -> Property
propAllowedPalLengthChar settings = forAll stringGenerator $ \originalString ->
    all
        (isAllowedPalLength settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

propAllowedPalLengthDNA :: Settings -> Property
propAllowedPalLengthDNA settings = forAll dnaGenerator $ \dnaSeq ->
    all
        (isAllowedPalLength settings)
        ( createCombinator
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

isAllowedPalLength :: Settings -> Palindrome -> Bool
isAllowedPalLength settings pal = case lengthMod settings of
    (l, Just u) -> palLength pal >= l && palLength pal <= u
    (l, Nothing) -> palLength pal >= l