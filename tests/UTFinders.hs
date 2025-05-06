module UTFinders where

import Test.HUnit (Test (..), (~:), (~?=))
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, forAll)

import Data.Algorithms.Palindromes.DNA (DNA (A, C, G, T), dnaToChar)
import Data.Algorithms.Palindromes.Finders
    ( Complexity (ComQuadratic)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    , findPalindromes
    )
import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (Palindrome, palRange, palRangeInText, palText)
    )

instance Arbitrary DNA where
    arbitrary = elements [A, T, C, G]

testListFinders =
    [ testFinderPlain
    , testFinderText
    , testFinderDNA
    , testFinderWord
    ]

testFinderPlain =
    "testFinderPlain"
        ~: [ ( Palindrome
                { palRange = (0, 1)
                , palText = "a"
                , palRangeInText = (0, 1)
                }
             )
           , ( Palindrome
                { palRange = (0, 3)
                , palText = "aba"
                , palRangeInText = (0, 3)
                }
             )
           , ( Palindrome
                { palRange = (0, 3)
                , palText = "a"
                , palRangeInText = (2, 3)
                }
             )
           ]
        ~?= findPalindromes VarPlain (ComQuadratic 0 0) (0, Nothing) "aba"

testFinderText =
    "testFinderText"
        ~: [ ( Palindrome
                { palRange = (0, 1)
                , palText = "a"
                , palRangeInText = (0, 1)
                }
             )
           , ( Palindrome
                { palRange = (0, 3)
                , palText = "ab'A"
                , palRangeInText = (0, 4)
                }
             )
           , ( Palindrome
                { palRange = (2, 3)
                , palText = "A"
                , palRangeInText = (3, 4)
                }
             )
           ]
        ~?= findPalindromes VarText (ComQuadratic 0 0) (0, Nothing) "ab'A"

testFinderWord =
    "testFinderWord"
        ~: [ ( Palindrome
                { palRange = (0, 1)
                , palText = "aba"
                , palRangeInText = (0, 3)
                }
             )
           , ( Palindrome
                { palRange = (0, 3)
                , palText = "aba' bbb\n aba"
                , palRangeInText = (0, 13)
                }
             )
           , ( Palindrome
                { palRange = (2, 3)
                , palText = "aba"
                , palRangeInText = (10, 13)
                }
             )
           ]
        ~?= findPalindromes VarWord (ComQuadratic 0 0) (0, Nothing) "aba' bbb\n aba"

testFinderDNA =
    "testFinderDNA"
        ~: [ ( Palindrome
                { palRange = (0, 2)
                , palText = "AT"
                , palRangeInText = (0, 2)
                }
             )
           , ( Palindrome
                { palRange = (1, 3)
                , palText = "TA"
                , palRangeInText = (1, 3)
                }
             )
           ]
        ~?= findPalindromes VarDNA (ComQuadratic 0 0) (0, Nothing) "ATA"
