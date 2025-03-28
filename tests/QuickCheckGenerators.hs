module QuickCheckGenerators where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (..)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    )
import Data.Algorithms.Palindromes.Settings (Settings (..))
import Test.QuickCheck
    ( Gen
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
done - add string before and after genPal
done - uneven pal
done - add a random amount of pal in pals
- look into errors
done - look into gapped
- look into dna
done - look into non text
- look into word pals
done - look into a new function that takes a variant/settings
wont - somehow unit test all of this
- can all chars safely be generated?

note: right now gapped palindromes have gapped palinpals, is that bad?
-}

-- the maximum amount of palInPal depth the generated palindrome will have
-- a Depth of 1 gives a palindrome with one level of palindrome (pallap)
-- a Depth of 2 gives a palindrome with two levels of palindromes (pallappallap) etc
maxPalInPalGeneration :: Float
maxPalInPalGeneration = 5

{- flag handler and exported function -}
palStringGenerator :: Settings -> Gen String
palStringGenerator settings = do
    let (gap, error) = case complexity settings of
            ComQuadratic{gapSize = gap, maxError = error} -> (gap, error)
            _ -> (0, 0)
    case variant settings of
        -- varDNA -> genStandardPalString gap
        -- VarWord -> genStandardPalString gap
        VarText -> genStandardPalString puncStringGenerator gap
        _ -> genStandardPalString plainStringGenerator gap -- Plain palindromes

-- generates random strings for punctuation palindromes
-- these can be anything
puncStringGenerator :: Gen String
puncStringGenerator = arbitrary :: Gen String

-- generates random strings for plain palindromes
plainStringGenerator :: Gen String
plainStringGenerator = listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z']))

{- Generators for punctuation palindromes -}
-- generates either a string, palindrome or palInPal palindrome with random characters around them
genStandardPalString :: Gen String -> Int -> Gen String
genStandardPalString stringGenerator gap = do
    randomStart <- stringGenerator
    palGenerator <-
        oneof
            [ stringGenerator
            , generatePalindrome stringGenerator gap
            , genMultiPalInPal stringGenerator gap
            ]
    randomEnd <- stringGenerator
    return $ randomStart ++ "---" ++ palGenerator ++ "---" ++ randomEnd

-- generates a palindrome
generatePalindrome :: Gen String -> Int -> Gen String
generatePalindrome stringGenerator gap = do
    randomString <- stringGenerator
    palInPal stringGenerator gap 1 randomString

-- generates a palindrome with a random amount of palInPal depth
genMultiPalInPal :: Gen String -> Int -> Gen String
genMultiPalInPal stringGenerator gap = do
    randomString <- stringGenerator
    randomFloat <- genFloat
    let -- Scale and round off float to generate a reasonable amount of palindromes
        scaledFLoat = round $ maxPalInPalGeneration * randomFloat
    palInPal stringGenerator gap scaledFLoat randomString

-- generate a palindrome from string with Int amount of palindromes
-- a depth of 0 gives the input back, (pal)
-- a depth of 1 gives a palindrome with one level of palindrome (pallap)
-- a depth of 2 gives a palindrome with two levels of palindrome (pallappallap)
palInPal :: Gen String -> Int -> Int -> String -> Gen String
palInPal stringGenerator gap depth string = do
    unevenOrGap <- generateGap stringGenerator gap
    case depth of
        0 -> return string
        _ ->
            palInPal stringGenerator gap (depth - 1) $
                string ++ "-o-" ++ unevenOrGap ++ "-o-" ++ reverse string

-- generates a string with a max length of 'gapSetting'
-- the string will be used to make gapped palindromes or uneven palindrome
-- even pals are reprented by this string being empty
generateGap :: Gen String -> Int -> Gen String
generateGap stringGenerator gapSetting = do
    randomFloat <- genFloat
    randomString <- stringGenerator
    let maxGapLength = fromIntegral $ max gapSetting 1 -- the maxGaplength is always set to at least 1 as this accounts for uneven palindromes
        randomGapLength = round $ maxGapLength * randomFloat
    return $ take randomGapLength randomString -- generates a string of length 'randomGapLength'
