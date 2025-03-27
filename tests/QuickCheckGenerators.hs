module QuickCheckGenerators where

import Test.QuickCheck (Gen, Property, arbitrary, forAll, generate, listOf, oneof)
import Test.QuickCheck.Gen (elements, genFloat)

{-
todo:
- uneven pal
- random stuff around pals
- look into aprox and gapped
-}
-- generates either a string, palindrome or palInPal
genPalString :: Gen String
genPalString = do
    let notPalindrome = arbitrary :: Gen String
        palindrome = generatePalindrome
        _palInpal = genPalInPal
    oneof [notPalindrome, palindrome, _palInpal]

-- generates a palindrome with a palInPal property
genPalInPal :: Gen String
genPalInPal = do
    randomString <- arbitrary :: Gen String
    return $ palInPal 2 randomString

-- generate a palindrome from string with Int amount of palindromes
-- an Int of 0 gives the input back, (pal)
-- an Int of 1 gives a palindrome with one level of palindrome (pallap)
-- an Int of 2 gives a palindrome with two levels of palindrome (pallappallap)
palInPal :: Int -> String -> String
palInPal i string =
    case i of
        0 -> string
        _ -> palInPal (i - 1) $ string ++ reverse string

-- generates a palindrome
generatePalindrome :: Gen String
generatePalindrome = do
    txt <- arbitrary :: Gen String
    return $ txt ++ reverse txt
