module QuickCheckProperties (propertyList) where

import Data.Char (isAlphaNum, isSpace, readLitChar, toLower)
import Data.Maybe (fromJust)
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , Property
    , arbitrary
    , choose
    , counterexample
    , elements
    , forAll
    , label
    , listOf
    , suchThat
    )

import Data.Algorithms.Palindromes.DNA (DNA (A, C, G, T), charToDNA, dnaToChar)
import Data.Algorithms.Palindromes.Finders
    ( Complexity (ComLinear, ComQuadratic)
    , Variant (VarDNA, VarPlain, VarPunctuation, VarText, VarWord)
    , findPalindromes
    )
import Data.Algorithms.Palindromes.PalEq (PalEq (..))
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..), getLength)
import Data.Algorithms.Palindromes.RangeFunctions (rangeToLength)
import Data.Algorithms.Palindromes.Settings
    ( Settings (..)
    )
import QuickCheckGenerators (generatePalindromes)
import QuickCheckSettings (settingsList)

-- List of to-be-tested properties, where each property is connected to all the settings
propertyList :: [Property]
propertyList =
    -- Property 1
    map propValidPalindromeRangeAndText settingsList
        -- Property 2
        ++ map propValidPalindromeReverse settingsList
        -- Property 3
        ++ map propValidPalLength settingsList
        -- Property 4
        ++ map propValidBoundaries settingsList
        -- Property 5
        ++ map propValidPalRange settingsList
        -- Property 6
        ++ map propAllowedPalLength settingsList

-- | Makes a Gen String based on the variant that is being used
stringGenerator :: Settings -> Gen String
stringGenerator = generatePalindromes

-- | Filters the non-alphabetic characters from the input, before converting everything to lowercase
cleanOriginalString :: String -> String
cleanOriginalString string = map toLower (filter (\a -> isAlphaNum a || isSpace a) string)

-- Property 1 ---------------------------------------------------------

-- | Test if all the generated Palindrome objects have a valid text related to the range property.
propValidPalindromeRangeAndText :: Settings -> Property
propValidPalindromeRangeAndText settings = counterexample (show settings ++ " property 1") $ forAll (stringGenerator settings) $ \originalString ->
    all
        (`checkPalRangeAndText` originalString)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

{- | Check that taking the substring of the original text described by the start and end of the palRange
property is equal to the palText property
-}
checkPalRangeAndText :: Palindrome -> String -> Bool
checkPalRangeAndText (Palindrome _ "" (x, y)) _ = x == y
checkPalRangeAndText (Palindrome _ palText (start, end)) originalString = palText == substringFromRange
  where
    substringFromRange = take (end - start) (drop start originalString)

-- Property 2 ---------------------------------------------------------

-- | Check that a found character palindrome is actually a palindrome
propValidPalindromeReverse :: Settings -> Property
propValidPalindromeReverse settings = counterexample (show settings ++ " property 2") $ forAll (stringGenerator settings) $ \originalString ->
    all
        (extractPalEq settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

extractPalEq :: Settings -> Palindrome -> Bool
extractPalEq settings pal = case variant settings of
    VarWord ->
        checkMismatches errors $
            removeGap gapLength (words (cleanOriginalString (palText pal)))
    VarPlain -> checkMismatches errors $ removeGap gapLength (palText pal)
    VarDNA ->
        checkMismatches errors $
            removeGap gapLength (map (fromJust . charToDNA) (palText pal))
    _ ->
        checkMismatches errors $ removeGap gapLength (cleanOriginalString (palText pal))
  where
    (gapLength, errors) = case complexity settings of
        ComQuadratic gap err -> (gap, err)
        ComLinear -> (0, 0)

-- | Checks if the character string is a palindrome, taking gaps and errors into account
checkMismatches :: (PalEq a) => Int -> [a] -> Bool
checkMismatches errors pal' = mismatches <= errors
  where
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
propValidPalLength :: Settings -> Property
propValidPalLength settings = counterexample (show settings ++ " property 3") $ forAll (stringGenerator settings) $ \originalString ->
    all
        (validPalLength settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Checks for every palindrome variant if the palLength corresponds to the length of palText
validPalLength :: Settings -> Palindrome -> Bool
validPalLength settings pal = case variant settings of
    VarWord ->
        length (words (cleanOriginalString (palText pal))) == getLength pal
    VarPlain -> length (palText pal) == getLength pal
    VarDNA -> length (palText pal) == getLength pal
    _ -> length (cleanOriginalString $ palText pal) == getLength pal

-- Property 4 ---------------------------------------------------------

-- | Property for testing if the palindrome range of a character palindrome corresponds to the palindrome length
propValidBoundaries :: Settings -> Property
propValidBoundaries settings = counterexample (show settings ++ " property 4") $ forAll (stringGenerator settings) $ \originalString ->
    all
        (checkValidBoundaries settings originalString)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Tests if the palindrome range of a palindrome corresponds to the palindrome length
checkValidBoundaries :: Settings -> String -> Palindrome -> Bool
checkValidBoundaries settings inputString pal = case variant settings of
    VarWord -> countWordsInRange (palRangeInText pal) inputString == getLength pal
    VarText ->
        let (s, e) = palRangeInText pal
        in  e - s - amountOfNonAlpha 0 (palText pal) == getLength pal
    VarPunctuation ->
        let (s, e) = palRangeInText pal
        in  e - s - amountOfNonAlpha 0 (palText pal) == getLength pal
    _ -> let (s, e) = palRangeInText pal in e - s == getLength pal

-- | Counts the amount of words that are in the substring of the input string corresponding with the given range
countWordsInRange :: (Int, Int) -> String -> Int
countWordsInRange (s, e) inputString = length . words $ cleanOriginalString $ take (e - s) (drop s inputString)

-- | Counts the amount of non-alphabetic characters in the string
amountOfNonAlpha :: Int -> String -> Int
amountOfNonAlpha acc [] = acc
amountOfNonAlpha acc (x : xs)
    | not (isAlphaNum x) = amountOfNonAlpha (acc + 1) xs
    | otherwise = amountOfNonAlpha acc xs

-- Property 5 ---------------------------------------------------------

-- | Tests if the range boundaries of a character palindrome are not outside the bounds of the string
propValidPalRange :: Settings -> Property
propValidPalRange settings = counterexample (show settings ++ " property 5") $ forAll (stringGenerator settings) $ \originalString ->
    all
        (\pal -> fst (palRangeInText pal) >= 0 && snd (palRangeInText pal) <= length originalString)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- Property 6 ---------------------------------------------------------

-- | Property for testing if the length of a character palindrome is allowed by the specified minimum and maximum length
propAllowedPalLength :: Settings -> Property
propAllowedPalLength settings = counterexample (show settings ++ " property 6") $ forAll (stringGenerator settings) $ \originalString ->
    all
        (isAllowedPalLength settings)
        ( findPalindromes
            (variant settings)
            (complexity settings)
            (lengthMod settings)
            originalString
        )

-- | Checks if the length of a palindrome is between the minimum and maximum length
isAllowedPalLength :: Settings -> Palindrome -> Bool
isAllowedPalLength settings pal = case lengthMod settings of
    (l, Just u) ->
        getLength pal >= l && getLength pal <= u
            || palText pal == ""
    (l, Nothing) -> getLength pal >= l || palText pal == ""
