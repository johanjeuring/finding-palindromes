module QuickCheckGenerators where
    
import Test.QuickCheck (Gen, Property, arbitrary, forAll, listOf, oneof, generate)
import Test.QuickCheck.Gen (genFloat, elements)
{- 
idea: generate a palindrome by randomly adding either characters or palindromes
which will place either characters or palindromes with a given frequency to generate a final string
for quickcheck we have:
    oneof <list of generators>
    frequency :: [(Int, Gen a)] -> Gen a

-}
-- builds a string of a random length by choosing either a character or a palindrome each time
genPalString :: Gen String
genPalString = do
    let
        char = listOf (elements ['a'..'z'])
        pal = generatePalindrome
        --listLength = 100 * genFloat
        --randomList = listOf listLength
    oneof [char, pal]

-- generates a palindrome
generatePalindrome:: Gen String
generatePalindrome = do
    txt <- listOf (elements ['a'..'z'])
    reverseTxt <- reverse txt
    return txt ++ reverseTxt
