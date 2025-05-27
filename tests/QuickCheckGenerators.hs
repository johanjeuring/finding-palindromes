module QuickCheckGenerators
    ( generatePalindromes
    ) where

import Data.List (intercalate)
import Data.Vector (Vector (..), fromList, toList, (//))
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitrary
    , choose
    , chooseInt
    , forAll
    , generate
    , listOf
    , oneof
    , suchThat
    , vectorOf
    )
import Test.QuickCheck.Gen (elements, genFloat)

import Data.Algorithms.Palindromes.DNA (DNA (..), dnaToChar)
import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , OutputFormat (..)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    )
import Data.Algorithms.Palindromes.PalEq (PalEq (..))
import Data.Algorithms.Palindromes.Settings (Settings (..))

maxPalInPalGeneration, minWordLength, maxWordLength :: Int
-- the maximum amount of palInPal depth the generated palindrome will have
-- a Depth of 1 gives a palindrome with one level of palindrome (pallap)
-- a Depth of 2 gives a palindrome with two levels of palindromes (pallappallap) etc
maxPalInPalGeneration = 2
-- The minimum and maximum length of the generated words
minWordLength = 1
maxWordLength = 7

-- | Constructs a Gen String for palindromes, based on the algorithm settings being used
generatePalindromes :: Settings -> Gen String
generatePalindromes settings = case variant settings of
    VarPlain -> generatePalindromeString id plainCharGenerator settings
    VarDNA -> map dnaToChar <$> generatePalindromeString compDNA dnaCharGenerator settings
    VarWord -> wordToString $ generatePalindromeString id wordGenerator settings
    _ -> generatePalindromeString id puncCharGenerator settings

-- | Converts the DNA datatype to it's complement
compDNA :: DNA -> DNA
compDNA A = T
compDNA T = A
compDNA C = G
compDNA G = C
compDNA N = N

-- | Converts a Gen [String] to a Gen String by concatenating the strings in the list with a space
wordToString :: Gen [[Char]] -> Gen String
wordToString gen = do unwords <$> gen

-- | Generates random strings for punctuation palindromes, these can be anything
puncCharGenerator :: Gen Char
puncCharGenerator = choose (' ', '~') `suchThat` (`notElem` ['\\', '"', ' ', '\n'])

-- | Generates random strings for plain palindromes
alphabetPlain = ['a' .. 'z'] ++ ['A' .. 'Z']

plainCharGenerator :: Gen Char
plainCharGenerator = elements alphabetPlain

-- makes DNA an instance of Arbitrary, so random DNA strings can be generated
instance Arbitrary DNA where
    arbitrary = elements [A, T, C, G]

-- | Simple Gen [DNA]
dnaCharGenerator :: Gen DNA
dnaCharGenerator = arbitrary :: Gen DNA

-- | Randomly generates one word with length between 2 and 7
wordGenerator :: Gen [Char]
wordGenerator = do
    randomWordLength <- chooseInt (minWordLength, maxWordLength)
    vectorOf randomWordLength $
        choose (' ', '~') `suchThat` (`notElem` ['\\', '"', ' ', '\n'])

{- | Generates either a string, palindrome or palInPal palindrome with random characters
around them. The function passes a function palComp all the way down to palInPal. This is
a bit weird, but defining a complement function in the PalEq datatype means that the (=:=)
function must be injective, which is an unnecessary restriction otherwise.
-}
generatePalindromeString
    :: (Arbitrary a, PalEq a) => (a -> a) -> Gen a -> Settings -> Gen [a]
generatePalindromeString palComp charGenerator settings = do
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
            , addErrors error charGenerator $ generatePalindrome palComp charGenerator gap
            , addErrors error charGenerator $ multiPalInPal palComp charGenerator gap
            ]
    -- generate random string to add noise behind the palindrome
    randomEnd <- listOf charGenerator
    return $ randomStart ++ palGenerator ++ randomEnd

-- | Generates a palindrome
generatePalindrome :: (Arbitrary a, PalEq a) => (a -> a) -> Gen a -> Int -> Gen [a]
generatePalindrome palComp charGenerator gap = do
    randomString <- listOf charGenerator
    palInPal palComp charGenerator gap 1 randomString

-- | generates a palindrome with a random amount of palInPal depth, note that a non palindrome can be generated if the random number is 0
multiPalInPal :: (Arbitrary a, PalEq a) => (a -> a) -> Gen a -> Int -> Gen [a]
multiPalInPal palComp charGenerator gap = do
    randomString <- listOf charGenerator
    palInPalDepth <- choose (0, maxPalInPalGeneration) -- generate a random int between 0 and maxDepth
    palInPal palComp charGenerator gap palInPalDepth randomString

{- | Generate a palindrome from string with Int amount of palindromes -
a depth of 0 gives the input back, (pal) -
a depth of 1 gives a palindrome with one level of palindrome (pallap) -
a depth of 2 gives a palindrome with two levels of palindrome (pallappallap)
-}
palInPal :: (Arbitrary a, PalEq a) => (a -> a) -> Gen a -> Int -> Int -> [a] -> Gen [a]
palInPal palComp charGenerator gap depth string = do
    case depth of
        0 -> return string
        1 -> do
            unevenOrGap <- generateGap charGenerator gap -- allows for uneven palindromes and gaps
            return $ string ++ unevenOrGap ++ reversePal palComp string
        _ -> do
            uneven <- generateGap charGenerator 1 -- allows for uneven palInPals
            palInPal palComp charGenerator gap (depth - 1) $
                string ++ uneven ++ reversePal palComp string

reversePal :: (PalEq a) => (a -> a) -> [a] -> [a]
reversePal palComp = reverse . map palComp

{- | generates a string with a max length of 'gapSetting' -
the string will be used to make gapped palindromes or uneven palindrome -
even pals are reprented by this string being empty
-}
generateGap :: (Arbitrary a) => Gen a -> Int -> Gen [a]
generateGap charGenerator gapSetting = do
    let maxGapLength = fromIntegral $ max gapSetting 1 -- the maxGaplength is always set to at least 1 to account for uneven palindromes
    randomGapLength <- choose (0, maxGapLength)
    vectorOf randomGapLength charGenerator -- generates a string of length 'randomGapLength'

-- | Generate x amount of errors in a string
addErrors :: Int -> Gen a -> Gen [a] -> Gen [a]
addErrors error charGenerator palGenerator = case error of
    0 -> palGenerator
    _ -> do
        randomError <- choose (0, error)
        _palGenerator <- palGenerator
        -- generate x indices on which the errors will be applied
        errorIndices <- vectorOf randomError $ choose (0, max 0 $ -1 + length _palGenerator)
        -- generate x random characters to replace the characters at the error indices
        replacementChars <- vectorOf randomError charGenerator
        let -- replace the characters at the error indices with the replacement characters
            -- we do this by converting into and from a vector
            errorZip = zip errorIndices replacementChars
            replaceErrors = toList $ replaceErrors' errorZip $ fromList _palGenerator
            replaceErrors' :: [(Int, a)] -> Vector a -> Vector a
            replaceErrors' zip _palGenerator = _palGenerator // zip
        if null _palGenerator || randomError == 0 then palGenerator else return replaceErrors


