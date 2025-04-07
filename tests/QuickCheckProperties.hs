module QuickCheckProperties where

import Data.Algorithms.Palindromes.DNA (DNA (A, C, G, T), charToDNA, dnaToChar)
import Data.Algorithms.Palindromes.Finders
    ( Complexity (ComLinear, ComQuadratic)
    , Variant (VarDNA, VarPlain, VarPunctuation, VarText, VarWord)
    , findPalindromes
    )
import Data.Algorithms.Palindromes.PalEq (PalEq (..))
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..))
import Data.Algorithms.Palindromes.Settings
    ( Settings (..)
    )
import Data.Char (isAlpha, readLitChar, toLower)
import PalindromeMethods (longestTextPalindrome)
import QuickCheckGenerators
    ( generateDNAPalindrome
    , generatePlainPalindrome
    , generatePunctuationPal
    , generateWordPalindrome
    )
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

propertyList :: [Property]
propertyList =
    map propValidPalLengthChar settingsListChar
        ++ map propValidPalLengthDNA settingsListDNA

-- List of to-be-tested properties, where each property is connected to all the settings
propertyListx :: [Property]
propertyListx =
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

-- | Makes a Gen String based on the variant that is being used
stringGenerator :: Settings -> Gen String
stringGenerator settings = case variant settings of
    VarWord -> generateWordPalindrome settings
    VarPlain -> generatePlainPalindrome settings
    _ -> generatePunctuationPal settings

-- | Makes a Gen DNA based on the variant that is being used
dnaGenerator :: Settings -> Gen [DNA]
dnaGenerator = generateDNAPalindrome

-- | Filters the non-alphabetic characters from the input, before converting everything to lowercase
cleanOriginalString :: String -> String
cleanOriginalString string = map toLower (filter isAlpha string)

-- Property 1 ---------------------------------------------------------

-- | Test if all the generated Palindrome objects have a valid text related to the range property. Works for the palindrome variants that work with Char
propValidPalindromeRangeAndTextChar :: Settings -> Property
propValidPalindromeRangeAndTextChar settings = forAll (stringGenerator settings) $ \originalString ->
    all
        (`checkPalRangeAndText` originalString)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Test if all the generated Palindrome objects have a valid text related to the range property. Works for the DNA palindrome-variant
propValidPalindromeRangeAndTextDNA :: Settings -> Property
propValidPalindromeRangeAndTextDNA settings = forAll (dnaGenerator settings) $ \dnaSeq ->
    all
        (`checkPalRangeAndText` map dnaToChar dnaSeq)
        ( findPalindromes
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

-- | Check that a found character palindrome is actually a palindrome
propValidPalindromeReverseChar :: Settings -> Property
propValidPalindromeReverseChar settings = forAll (stringGenerator settings) $ \originalString ->
    all
        (isPalindromeGapsErrorsChar settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Check that a found DNA palindrome is actually a palindrome
propValidPalindromeReverseDNA :: Settings -> Property
propValidPalindromeReverseDNA settings = forAll (dnaGenerator settings) $ \dnaSeq ->
    all
        (isPalindromeGapsErrorsDNA settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

-- | Checks if the character string is a palindrome, taking gaps and errors into account
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

-- | Checks if the DNA string is a palindrome, taking gaps and errors into account
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
            , not $ (pal' !! i) =:= (pal' !! (length pal' - 1 - i))
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

-- | Tests if the palLength of a character palindrome corresponds to the palText
propValidPalLengthChar :: Settings -> Property
propValidPalLengthChar settings = forAll (stringGenerator settings) $ \originalString ->
    all
        (validPalLength settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Tests if the palLength of a DNA palindrome corresponds to the palText
propValidPalLengthDNA :: Settings -> Property
propValidPalLengthDNA settings = forAll (dnaGenerator settings) $ \dnaSeq ->
    all
        (validPalLength settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

-- | Checks for every palindrome variant if the palLength corresponds to the length of palText
validPalLength :: Settings -> Palindrome -> Bool
validPalLength settings pal = case variant settings of
    VarWord -> length (words (palText pal)) == palLength pal
    VarPlain -> length (palText pal) == palLength pal
    VarDNA -> length (palText pal) == palLength pal
    _ -> length (cleanOriginalString $ palText pal) == palLength pal

-- Property 4 ---------------------------------------------------------

-- | Property for testing if the palindrome range of a character palindrome corresponds to the palindrome length
propValidBoundariesChar :: Settings -> Property
propValidBoundariesChar settings = forAll (stringGenerator settings) $ \originalString ->
    all
        (checkValidBoundariesChar settings originalString)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Property for testing if the palindrome range of a DNA palindrome corresponds to the palindrome length
propValidBoundariesDNA :: Settings -> Property
propValidBoundariesDNA settings = forAll (dnaGenerator settings) $ \dnaSeq ->
    all
        (checkValidBoundariesDNA settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

-- | Tests if the palindrome range of a character palindrome corresponds to the palindrome length
checkValidBoundariesChar :: Settings -> String -> Palindrome -> Bool
checkValidBoundariesChar settings inputString pal = case variant settings of
    VarPlain -> let (s, e) = palRange pal in e - s == palLength pal
    VarWord -> countWordsInRange (palRange pal) inputString == palLength pal
    _ ->
        let (s, e) = palRange pal in e - s - amountOfNonAlpha 0 (palText pal) == palLength pal

-- | Tests if the palindrome range of a DNA palindrome corresponds to the palindrome length
checkValidBoundariesDNA :: Settings -> Palindrome -> Bool
checkValidBoundariesDNA settings pal = let (s, e) = palRange pal in e - s == palLength pal

-- | Counts the amount of words that are in the substring of the input string corresponding with the given range
countWordsInRange :: (Int, Int) -> String -> Int
countWordsInRange (s, e) inputString = length . words $ take (e - s) (drop s inputString)

-- | Counts the amount of non-alphabetic characters in the string
amountOfNonAlpha :: Int -> String -> Int
amountOfNonAlpha acc [] = acc
amountOfNonAlpha acc (x : xs)
    | not (isAlpha x) = amountOfNonAlpha (acc + 1) xs
    | otherwise = amountOfNonAlpha acc xs

-- Property 5 ---------------------------------------------------------

-- | Tests if the range boundaries of a character palindrome are not outside the bounds of the string
propValidPalRangeChar :: Settings -> Property
propValidPalRangeChar settings = forAll (stringGenerator settings) $ \originalString ->
    all
        (\pal -> fst (palRange pal) >= 0 && snd (palRange pal) <= length originalString)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Tests if the range boundaries of a DNA palindrome are not outside the bounds of the string
propValidPalRangeDNA :: Settings -> Property
propValidPalRangeDNA settings = forAll (dnaGenerator settings) $ \dnaSeq ->
    all
        (\pal -> fst (palRange pal) >= 0 && snd (palRange pal) <= length dnaSeq)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

-- Property 6 ---------------------------------------------------------

-- | Property for testing if the length of a character palindrome is allowed by the specified minimum and maximum length
propAllowedPalLengthChar :: Settings -> Property
propAllowedPalLengthChar settings = forAll (stringGenerator settings) $ \originalString ->
    all
        (isAllowedPalLength settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Property for testing if the length of a DNA palindrome is allowed by the specified minimum and maximum length
propAllowedPalLengthDNA :: Settings -> Property
propAllowedPalLengthDNA settings = forAll (dnaGenerator settings) $ \dnaSeq ->
    all
        (isAllowedPalLength settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            (map dnaToChar dnaSeq)
        )

-- | Checks if the length of a palindrome is between the minimum and maximum length
isAllowedPalLength :: Settings -> Palindrome -> Bool
isAllowedPalLength settings pal = case lengthMod settings of
    (l, Just u) -> palLength pal >= l && palLength pal <= u || palText pal == ""
    (l, Nothing) -> palLength pal >= l || palText pal == ""