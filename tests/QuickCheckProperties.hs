{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring -}

module QuickCheckProperties (propertyList) where

import Data.Char (isAlphaNum, isSpace, readLitChar, toLower)
import Data.Foldable.Levenshtein (levenshtein', levenshteinDistance')
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
    , ioProperty
    , label
    , listOf
    , suchThat
    )

import Data.Algorithms.Palindromes.DNA (DNA (A, C, G, T), charToDNA, dnaToChar)
import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , Variant (VarDNA, VarPlain, VarPunctuation, VarText, VarWord)
    , findPalindromes
    )
import Data.Algorithms.Palindromes.Internal.RangeFunctions (Range, rangeToLength)
import Data.Algorithms.Palindromes.PalEq (PalEq (..))
import Data.Algorithms.Palindromes.Palindrome (Palindrome (..), getLength)
import Data.Algorithms.Palindromes.Settings
    ( Settings (..)
    )
import Data.Algorithms.Palindromes.Streaming (findPalindromesVisualised)
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
        -- Property 7
        ++ map propStreamSameResult settingsList

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
            (algorithm settings)
            (minLength settings)
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
            (algorithm settings)
            (minLength settings)
            originalString
        )

extractPalEq :: Settings -> Palindrome -> Bool
extractPalEq settings pal = case algorithm settings of
    AlgApproximate _ _ -> case variant settings of
        VarWord -> isApproximatePalindrome $ words (cleanOriginalString (palText pal))
        VarPlain -> isApproximatePalindrome $ palText pal
        VarDNA -> isApproximatePalindrome $ map (fromJust . charToDNA) (palText pal)
        _ -> isApproximatePalindrome $ cleanOriginalString (palText pal)
    _ -> case variant settings of
        VarWord -> isPalindrome $ words (cleanOriginalString (palText pal))
        VarPlain -> isPalindrome $ palText pal
        VarDNA -> isPalindrome $ map (fromJust . charToDNA) (palText pal)
        _ -> isPalindrome $ cleanOriginalString (palText pal)
  where
    (gapSize, maxErrors) = case algorithm settings of
        AlgQuadratic gap err -> (gap, err)
        AlgApproximate gap err -> (gap, err)
        AlgLinear -> (0, 0)

    {- if any of the possible removed gaps has levenshteinDistance with its reverse is
    less than 2 * maxErrors we have an a valid approximate palindromes. This is because
    2 * maxErrors is equivalent to the levenshteinDistance -}
    isApproximatePalindrome :: (PalEq a) => [a] -> Bool
    isApproximatePalindrome input =
        any
            (<= 2 * maxErrors)
            ( zipWith
                (levenshteinDistance' (=:=))
                (allPossibleGapless gapSize maxErrors input)
                (map reverse (allPossibleGapless gapSize maxErrors input))
            )
    isPalindrome :: (PalEq a) => [a] -> Bool
    isPalindrome input = checkMismatches maxErrors $ removeGap 0 gapSize input

-- | for approximate palindromes the gap can be shifted due to insertions, so we need all.
allPossibleGapless :: (PalEq a) => Int -> Int -> [a] -> [[a]]
allPossibleGapless 0 _ palindrome = [palindrome]
allPossibleGapless gapSize 0 palindrome = [removeGap 0 gapSize palindrome]
allPossibleGapless gapSize maxErrors palindrome =
    map (\e -> removeGap e gapSize palindrome) [-maxErrors .. maxErrors]

-- | Checks if the character string is a palindrome, taking gapSize and maxErrors into account
checkMismatches :: (PalEq a) => Int -> [a] -> Bool
checkMismatches maxErrors pal' = mismatches <= maxErrors
  where
    mismatches =
        length
            [ ()
            | i <- [0 .. (length pal' `div` 2) - 1]
            , not $ (pal' !! i) =:= (pal' !! (length pal' - 1 - i))
            ]

-- | Removes gap from palindrome
removeGap :: Int -> Int -> [a] -> [a]
removeGap offset gapSize palindrome = take start palindrome ++ drop end palindrome
  where
    start = (length palindrome - adjustedGap + offset) `div` 2
    end = start + adjustedGap
    adjustGap = if even offset then not adjustWhenOddOffset else adjustWhenOddOffset
    adjustWhenOddOffset = even (length palindrome) == even gapSize || gapSize == 0
    adjustedGap =
        if adjustGap
            then gapSize - 1
            else gapSize

-- Property 3 ---------------------------------------------------------

-- | Tests if the palLength of a character palindrome corresponds to the palText
propValidPalLength :: Settings -> Property
propValidPalLength settings = counterexample (show settings ++ " property 3") $ forAll (stringGenerator settings) $ \originalString ->
    all
        (validPalLength settings)
        ( findPalindromes
            (variant settings)
            (algorithm settings)
            (minLength settings)
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
            (algorithm settings)
            (minLength settings)
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
countWordsInRange :: Range -> String -> Int
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
            (algorithm settings)
            (minLength settings)
            originalString
        )

-- Property 6 ---------------------------------------------------------

-- | Property for testing if the length of a character palindrome is allowed by the specified minimum length
propAllowedPalLength :: Settings -> Property
propAllowedPalLength settings = counterexample (show settings ++ " property 6") $ forAll (stringGenerator settings) $ \originalString ->
    all
        (isAllowedPalLength settings)
        ( findPalindromes
            (variant settings)
            (algorithm settings)
            (minLength settings)
            originalString
        )

-- | Checks if the length of a palindrome is greater than the minimum length
isAllowedPalLength :: Settings -> Palindrome -> Bool
isAllowedPalLength settings pal = case minLength settings of
    l ->
        getLength pal >= l
            || palText pal == ""

-- Property 7 ---------------------------------------------------------

{- | Check that finding palindromes with intermediate visualisation
returns the same as the normal findPalindromes
-}
propStreamSameResult :: Settings -> Property
propStreamSameResult settings = counterexample (show settings ++ " property 7") $ forAll (stringGenerator settings) $ \originalString ->
    ioProperty $ do
        let pure =
                findPalindromes
                    (variant settings)
                    (algorithm settings)
                    (minLength settings)
                    originalString
        streamed <-
            findPalindromesVisualised
                (variant settings)
                (algorithm settings)
                (minLength settings)
                False
                originalString
                (const $ return ()) -- Don't do anything in the visualisation step
        return (pure == streamed)
