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
import Data.List (intercalate)
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitrary
    , choose
    , forAll
    , generate
    , listOf
    , oneof
    , suchThat
    , vectorOf
    )
import Test.QuickCheck.Gen (elements, genFloat)

{-
TODOs:
- apply and test addErrors function
done - change [a] gen to a gen
done - make new word pal generators
- look into which characters make a new line character for word palindromes so these are not generated in strings
- make the testing faster
- apply haddock style comments
-}

-- the maximum amount of palInPal depth the generated palindrome will have
-- a Depth of 1 gives a palindrome with one level of palindrome (pallap)
-- a Depth of 2 gives a palindrome with two levels of palindromes (pallappallap) etc
maxPalInPalGeneration :: Float
maxPalInPalGeneration = 5

generatePunctuationPal :: Settings -> Gen String
generatePunctuationPal = generatePalindromeString puncCharGenerator

generatePlainPalindrome :: Settings -> Gen String
generatePlainPalindrome = generatePalindromeString plainCharGenerator

generateDNAPalindrome :: Settings -> Gen [DNA]
generateDNAPalindrome = generatePalindromeString dnaCharGenerator

generateWordPalindrome :: Settings -> Gen String
generateWordPalindrome = wordToString . generatePalindromeString wordGenerator

wordToString :: Gen [[Char]] -> Gen String
wordToString gen = do unwords <$> gen

-- generates random strings for punctuation palindromes
-- these can be anything
puncCharGenerator :: Gen Char
puncCharGenerator = choose (' ', '~') `suchThat` (`notElem` ['\\', '"'])

-- generates random strings for plain palindromes
plainCharGenerator :: Gen Char
plainCharGenerator = elements (['a' .. 'z'] ++ ['A' .. 'Z'])

instance Arbitrary DNA where
    arbitrary = elements [A, T, C, G]

dnaCharGenerator :: Gen DNA
dnaCharGenerator = arbitrary :: Gen DNA

wordGenerator :: Gen [Char]
wordGenerator = do
    randomWordLength <- randomInt 2 7
    vectorOf randomWordLength $
        choose (' ', '~') `suchThat` (`notElem` ['\\', '"', ' ', '\n']) -- TODO are these all possible characters?

{- Generators for plain and punctuation palindromes -}
-- generates either a string, palindrome or palInPal palindrome with random characters around them
generatePalindromeString :: (Arbitrary a) => Gen a -> Settings -> Gen [a]
generatePalindromeString charGenerator settings = do
    -- get the gap and error settings from the complexity settings
    let (gap, error) = case complexity settings of
            ComQuadratic{gapSize = gap, maxError = error} -> (gap, error)
            _ -> (0, 0)
    -- generate random string to add noise in front of the palindrome
    randomStart <- listOf charGenerator
    -- generate the palindrome
    palGenerator <-
        oneof
            [ listOf charGenerator
            , generatePalindrome charGenerator gap -- TODO addErrors error $ generatePalindrome stringGenerator gap
            , multiPalInPal charGenerator gap
            ]
    -- generate random string to add noise behind the palindrome
    randomEnd <- listOf charGenerator
    return $ randomStart ++ palGenerator ++ randomEnd

-- generates a palindrome
generatePalindrome :: (Arbitrary a) => Gen a -> Int -> Gen [a]
generatePalindrome charGenerator gap = do
    randomString <- listOf charGenerator
    palInPal charGenerator gap 1 randomString

-- generates a palindrome with a random amount of palInPal depth
-- note that a non palindrome can be generated if the random float is 0
multiPalInPal :: (Arbitrary a) => Gen a -> Int -> Gen [a]
multiPalInPal charGenerator gap = do
    randomString <- listOf charGenerator
    palInPalDepth <- randomInt 0 maxPalInPalGeneration -- generate a random int between 0 and maxDepth
    palInPal charGenerator gap palInPalDepth randomString

-- generate a palindrome from string with Int amount of palindromes
-- a depth of 0 gives the input back, (pal)
-- a depth of 1 gives a palindrome with one level of palindrome (pallap)
-- a depth of 2 gives a palindrome with two levels of palindrome (pallappallap)
palInPal :: (Arbitrary a) => Gen a -> Int -> Int -> [a] -> Gen [a]
palInPal charGenerator gap depth string = do
    case depth of
        0 -> return string
        1 -> do
            unevenOrGap <- generateGap charGenerator gap -- allows for uneven palindromes and gaps
            return $ string ++ unevenOrGap ++ reverse string
        _ -> do
            uneven <- generateGap charGenerator 1 -- allows for uneven palInPals
            palInPal charGenerator gap (depth - 1) $
                string ++ uneven ++ reverse string

-- generates a string with a max length of 'gapSetting'
-- the string will be used to make gapped palindromes or uneven palindrome
-- even pals are reprented by this string being empty
generateGap :: (Arbitrary a) => Gen a -> Int -> Gen [a]
generateGap charGenerator gapSetting = do
    randomString <- listOf charGenerator
    let maxGapLength = fromIntegral $ max gapSetting 1 -- the maxGaplength is always set to at least 1 to account for uneven palindromes
    randomGapLength <- randomInt 0 maxGapLength
    return $ take randomGapLength randomString -- generates a string of length 'randomGapLength'

-- generate x amount of errors in a string
addErrors :: Int -> Gen a -> Gen [a]
addErrors error charGenerator = do
    randomString <- listOf charGenerator
    -- generate x indices on which the errors will be applied
    errorIndices <- vectorOf error $ randomInt 0 $ fromIntegral $ length randomString
    -- generate x random characters to replace the characters at the error indices
    replacementChars <- vectorOf error charGenerator
    let -- replacementChars = take error randomChars
        -- replace the characters at the error indices with the replacement characters
        replaceErrors :: [Int] -> [a] -> [a] -> [a]
        replaceErrors [] _ str = str
        replaceErrors (i : is) (e : es) str = replaceErrors is es $ take i str ++ [e] ++ drop (i + 1) str
    return $ replaceErrors errorIndices replacementChars randomString

-- int generator helper function
-- gets a minimum and maximum value and generates a random int between them
randomInt :: Float -> Float -> Gen Int
randomInt _min _max = do
    randomFloat <- genFloat
    let maxValue = _max * randomFloat
        randomInt = round $ max _min maxValue
    return randomInt
