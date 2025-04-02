module QuickCheckGenerators
    ( generatePunctuationPal
    , generatePlainPalindrome
    , generateDNAPalindrome
    , generateWordPalindrome
    ) where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (..)
    , Output (..)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    )
import Data.Algorithms.Palindromes.PalindromesUtils (Couplable (..), DNA (..))
import Data.Algorithms.Palindromes.Settings (Settings (..))
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitrary
    , forAll
    , generate
    , listOf
    , oneof
    , vectorOf
    )
import Test.QuickCheck.Gen (elements, genFloat)

{-
todo:
- look into errors,
-}

-- the maximum amount of palInPal depth the generated palindrome will have
-- a Depth of 1 gives a palindrome with one level of palindrome (pallap)
-- a Depth of 2 gives a palindrome with two levels of palindromes (pallappallap) etc
maxPalInPalGeneration :: Float
maxPalInPalGeneration = 5

generatePunctuationPal :: Settings -> Gen String
generatePunctuationPal = generatePalindromeString puncStringGenerator

generatePlainPalindrome :: Settings -> Gen String
generatePlainPalindrome = generatePalindromeString plainStringGenerator

generateDNAPalindrome :: Settings -> Gen [DNA]
generateDNAPalindrome = generatePalindromeString dnaStringGenerator

generateWordPalindrome :: Settings -> Gen String
generateWordPalindrome = generatePalindromeString wordStringGenerator

-- generates random strings for punctuation palindromes
-- these can be anything
puncStringGenerator :: Gen String
puncStringGenerator = arbitrary :: Gen String

-- generates random strings for plain palindromes
plainStringGenerator :: Gen String
plainStringGenerator = listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

instance Arbitrary DNA where
    arbitrary = elements [A, T, C, G]

dnaStringGenerator :: Gen [DNA]
dnaStringGenerator = arbitrary :: Gen [DNA]

randomWord :: Gen String
randomWord = do
    randomFloat <- genFloat
    randomString <- arbitrary :: Gen String
    let -- make a word of a random length between 2 and 7
        randomWordLength = max 2 $ round $ 7 * randomFloat
        randomWord = take randomWordLength randomString
    return randomWord

wordStringGenerator :: Gen [Char]
wordStringGenerator = do
    amountOfWords <- genFloat
    let -- make a word of a random length between 2 and 7
        randomWordAmount = round $ 5 * amountOfWords
    wordStringGenerator' randomWordAmount

wordStringGenerator' :: Int -> Gen [Char]
wordStringGenerator' i = do
    _randomWord <- randomWord
    case i of
        0 -> return []
        1 -> return _randomWord
        _ -> do
            newWord <- wordStringGenerator' (i - 1)
            return $ _randomWord ++ " " ++ newWord

{- Generators for plain and punctuation palindromes -}
-- generates either a string, palindrome or palInPal palindrome with random characters around them
generatePalindromeString :: (Arbitrary a) => Gen [a] -> Settings -> Gen [a]
generatePalindromeString stringGenerator settings = do
    -- get the gap and error settings from the complexity settings
    let (gap, error) = case complexity settings of
            ComQuadratic{gapSize = gap, maxError = error} -> (gap, error)
            _ -> (0, 0)
    -- generate random string to add noise in front of the palindrome
    randomStart <- stringGenerator
    -- generate the palindrome
    palGenerator <-
        oneof
            [ stringGenerator
            , generatePalindrome stringGenerator gap
            , multiPalInPal stringGenerator gap
            ]
    -- generate random string to add noise behind the palindrome
    randomEnd <- stringGenerator
    return $ randomStart ++ palGenerator ++ randomEnd

-- generates a palindrome
generatePalindrome :: (Arbitrary a) => Gen [a] -> Int -> Gen [a]
generatePalindrome stringGenerator gap = do
    randomString <- stringGenerator
    palInPal stringGenerator gap 1 randomString

-- generates a palindrome with a random amount of palInPal depth
-- note that a non palindrome can be generated if the random float is 0
multiPalInPal :: (Arbitrary a) => Gen [a] -> Int -> Gen [a]
multiPalInPal stringGenerator gap = do
    randomString <- stringGenerator
    randomFloat <- genFloat
    let -- Scale and round off float to generate a reasonable amount of palindromes
        scaledFLoat = round $ maxPalInPalGeneration * randomFloat
    palInPal stringGenerator gap scaledFLoat randomString

-- generate a palindrome from string with Int amount of palindromes
-- a depth of 0 gives the input back, (pal)
-- a depth of 1 gives a palindrome with one level of palindrome (pallap)
-- a depth of 2 gives a palindrome with two levels of palindrome (pallappallap)
palInPal :: (Arbitrary a) => Gen [a] -> Int -> Int -> [a] -> Gen [a]
palInPal stringGenerator gap depth string = do
    case depth of
        0 -> return string
        1 -> do
            unevenOrGap <- generateGap stringGenerator gap -- allows for uneven palindromes and gaps
            return $ string ++ unevenOrGap ++ reverse string
        _ -> do
            uneven <- generateGap stringGenerator 1 -- allows for uneven palInPals
            palInPal stringGenerator gap (depth - 1) $
                string ++ uneven ++ reverse string

-- generates a string with a max length of 'gapSetting'
-- the string will be used to make gapped palindromes or uneven palindrome
-- even pals are reprented by this string being empty
generateGap :: (Arbitrary a) => Gen [a] -> Int -> Gen [a]
generateGap stringGenerator gapSetting = do
    randomFloat <- genFloat
    randomString <- stringGenerator
    let maxGapLength = fromIntegral $ max gapSetting 1 -- the maxGaplength is always set to at least 1 as this accounts for uneven palindromes
        randomGapLength = round $ maxGapLength * randomFloat
    return $ take randomGapLength randomString -- generates a string of length 'randomGapLength'
