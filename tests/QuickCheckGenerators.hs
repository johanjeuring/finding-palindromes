module QuickCheckGenerators where

import Test.QuickCheck (Gen, Property, arbitrary, forAll, generate, listOf, oneof)
import Test.QuickCheck.Gen (elements, genFloat)

{-
todo:
done - add string before and after genPal
- uneven pal
done - add a random amount of pal in pals
- look into aprox and gapped
- somehow unit test all of this
-}
-- the maximum amount of palInPal depth the generated palindrome will have
-- a Depth of 1 gives a palindrome with one level of palindrome (pallap)
-- a Depth of 2 gives a palindrome with two levels of palindromes (pallappallap) etc
maxPalInPalGeneration :: Float
maxPalInPalGeneration = 5

-- generates either a string, palindrome or palInPal palindrome with radnom characters around them
genPalString :: Gen String
genPalString = do
    randomStart <- arbitrary :: Gen String
    palGenerator <-
        oneof [arbitrary :: Gen String, generatePalindrome, genSinglePalInPal, genMultiPalInPal]
    randomEnd <- arbitrary :: Gen String
    return $ randomStart ++ palGenerator ++ randomEnd

genMultiPalInPal :: Gen String
genMultiPalInPal = do
    randomString <- arbitrary :: Gen String
    randomFloat <- genFloat
    let -- Scale and round off float to generate a reasonable amount of palindromes
        scaledFLoat = round $ maxPalInPalGeneration * randomFloat
    return $ palInPal scaledFLoat randomString

-- generates a palindrome with a palInPal property
genSinglePalInPal :: Gen String
genSinglePalInPal = do
    randomString <- arbitrary :: Gen String
    return $ palInPal 2 randomString

-- generate a palindrome from string with Int amount of palindromes
-- a depth of 0 gives the input back, (pal)
-- a depth of 1 gives a palindrome with one level of palindrome (pallap)
-- a depth of 2 gives a palindrome with two levels of palindrome (pallappallap)
palInPal :: Int -> String -> String
palInPal depth string =
    case depth of
        0 -> string
        _ -> palInPal (depth - 1) $ string ++ reverse string

-- generates a palindrome
generatePalindrome :: Gen String
generatePalindrome = do
    randomString <- arbitrary :: Gen String
    return $ palInPal 1 randomString
