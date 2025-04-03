module UTCombinators where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (ComQuadratic)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    , createCombinator
    )
import Data.Algorithms.Palindromes.DNA (DNA (A, C, G, T), dnaToChar)
import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (Palindrome, palCenterIndex, palLength, palRange, palText)
    )
import Test.HUnit (Test (..), (~:), (~?=))
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, forAll)

instance Arbitrary DNA where
    arbitrary = elements [A, T, C, G]

testListCombinators =
    [ testCombinatorPlain
    , testCombinatorText
    , testCombinatorDNA
    , testCombinatorWord
    ]

testCombinatorPlain =
    "testCombinatorPlain"
        ~: [ ( Palindrome
                { palCenterIndex = 0
                , palLength = 0
                , palText = ""
                , palRange = (0, 0)
                }
             )
           , ( Palindrome
                { palCenterIndex = 1
                , palLength = 1
                , palText = "a"
                , palRange = (0, 1)
                }
             )
           , ( Palindrome
                { palCenterIndex = 2
                , palLength = 0
                , palText = ""
                , palRange = (1, 1)
                }
             )
           , ( Palindrome
                { palCenterIndex = 3
                , palLength = 3
                , palText = "aba"
                , palRange = (0, 3)
                }
             )
           , ( Palindrome
                { palCenterIndex = 4
                , palLength = 0
                , palText = ""
                , palRange = (2, 2)
                }
             )
           , ( Palindrome
                { palCenterIndex = 5
                , palLength = 1
                , palText = "a"
                , palRange = (2, 3)
                }
             )
           , ( Palindrome
                { palCenterIndex = 6
                , palLength = 0
                , palText = ""
                , palRange = (3, 3)
                }
             )
           ]
        ~?= createCombinator VarPlain (ComQuadratic 0 0) (0, Nothing) "aba"

testCombinatorText =
    "testCombinatorText"
        ~: [ ( Palindrome
                { palCenterIndex = 0
                , palLength = 0
                , palText = ""
                , palRange = (0, 0)
                }
             )
           , ( Palindrome
                { palCenterIndex = 1
                , palLength = 1
                , palText = "a"
                , palRange = (0, 1)
                }
             )
           , ( Palindrome
                { palCenterIndex = 2
                , palLength = 0
                , palText = ""
                , palRange = (1, 1)
                }
             )
           , ( Palindrome
                { palCenterIndex = 3
                , palLength = 3
                , palText = "ab'A"
                , palRange = (0, 4)
                }
             )
           , ( Palindrome
                { palCenterIndex = 4
                , palLength = 0
                , palText = ""
                , palRange = (3, 3)
                }
             )
           , ( Palindrome
                { palCenterIndex = 5
                , palLength = 1
                , palText = "A"
                , palRange = (3, 4)
                }
             )
           , ( Palindrome
                { palCenterIndex = 6
                , palLength = 0
                , palText = ""
                , palRange = (4, 4)
                }
             )
           ]
        ~?= createCombinator VarText (ComQuadratic 0 0) (0, Nothing) "ab'A"

testCombinatorWord =
    "testCombinatorWord"
        ~: [ ( Palindrome
                { palCenterIndex = 0
                , palLength = 0
                , palText = ""
                , palRange = (0, 0)
                }
             )
           , ( Palindrome
                { palCenterIndex = 1
                , palLength = 1
                , palText = "aba"
                , palRange = (0, 3)
                }
             )
           , ( Palindrome
                { palCenterIndex = 2
                , palLength = 0
                , palText = ""
                , palRange = (5, 5)
                }
             )
           , ( Palindrome
                { palCenterIndex = 3
                , palLength = 3
                , palText = "aba' bbb\n aba"
                , palRange = (0, 13)
                }
             )
           , ( Palindrome
                { palCenterIndex = 4
                , palLength = 0
                , palText = ""
                , palRange = (10, 10)
                }
             )
           , ( Palindrome
                { palCenterIndex = 5
                , palLength = 1
                , palText = "aba"
                , palRange = (10, 13)
                }
             )
           , ( Palindrome
                { palCenterIndex = 6
                , palLength = 0
                , palText = ""
                , palRange = (13, 13)
                }
             )
           ]
        ~?= createCombinator VarWord (ComQuadratic 0 0) (0, Nothing) "aba' bbb\n aba"

testCombinatorDNA =
    "testCombinatorDNA"
        ~: [ ( Palindrome
                { palCenterIndex = 0
                , palLength = 0
                , palText = ""
                , palRange = (0, 0)
                }
             )
           , ( Palindrome
                { palCenterIndex = 1
                , palLength = 2
                , palText = "AT"
                , palRange = (0, 2)
                }
             )
           , ( Palindrome
                { palCenterIndex = 2
                , palLength = 2
                , palText = "TA"
                , palRange = (1, 3)
                }
             )
           , ( Palindrome
                { palCenterIndex = 3
                , palLength = 0
                , palText = ""
                , palRange = (3, 3)
                }
             )
           ]
        ~?= createCombinator VarDNA (ComQuadratic 0 0) (0, Nothing) "ATA"

propValidPalindromeRangeAndTextPlain :: Property
propValidPalindromeRangeAndTextPlain = propValidPalindromeRangeAndText VarPlain
propValidPalindromeRangeAndTextText :: Property
propValidPalindromeRangeAndTextText = propValidPalindromeRangeAndText VarText
propValidPalindromeRangeAndTextWord :: Property
propValidPalindromeRangeAndTextWord = propValidPalindromeRangeAndText VarWord

-- | Test if all the generated Palindrome objects have a valid text related to the range property
propValidPalindromeRangeAndText :: Variant -> Property
propValidPalindromeRangeAndText variant = forAll (arbitrary :: Gen [Char]) $ \originalString ->
    all
        (`checkPalRangeAndText` originalString)
        (createCombinator variant (ComQuadratic 0 0) (0, Nothing) originalString)

propValidPalindromeRangeAndTextDNA :: Property
propValidPalindromeRangeAndTextDNA = forAll (arbitrary :: Gen [DNA]) $ \dna ->
    all
        (`checkPalRangeAndText` map dnaToChar dna)
        (createCombinator VarDNA (ComQuadratic 0 0) (0, Nothing) (map dnaToChar dna))

{- | Check that taking the substring of the original text described by the start and end of the palRange
property is equal to the palText property
-}
checkPalRangeAndText :: Palindrome -> String -> Bool
checkPalRangeAndText (Palindrome _ _ "" (x, y)) _ = x == y
checkPalRangeAndText (Palindrome _ _ palText (start, end)) originalString = palText == substringFromRange
  where
    substringFromRange = take (end - start) (drop start originalString)
