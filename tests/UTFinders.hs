module UTFinders (testListFinders) where

import Test.HUnit (Test (..), (~:), (~?=))
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, forAll)

import Data.Algorithms.Palindromes.DNA (DNA (A, C, G, T), dnaToChar)
import Data.Algorithms.Palindromes.Finders
    ( Complexity (ComQuadratic)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    , findPalindromes
    )
import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (Palindrome, palCenterIndex, palLength, palRange, palText)
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
                { palCenterIndex = 1
                , palLength = 1
                , palText = "a"
                , palRange = (0, 1)
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
                { palCenterIndex = 5
                , palLength = 1
                , palText = "a"
                , palRange = (2, 3)
                }
             )
           ]
        ~?= findPalindromes VarPlain (ComQuadratic 0 0) (0, Nothing) "aba"

testFinderText =
    "testFinderText"
        ~: [ ( Palindrome
                { palCenterIndex = 1
                , palLength = 1
                , palText = "a"
                , palRange = (0, 1)
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
                { palCenterIndex = 5
                , palLength = 1
                , palText = "A"
                , palRange = (3, 4)
                }
             )
           ]
        ~?= findPalindromes VarText (ComQuadratic 0 0) (0, Nothing) "ab'A"

testFinderWord =
    "testFinderWord"
        ~: [ ( Palindrome
                { palCenterIndex = 1
                , palLength = 1
                , palText = "aba"
                , palRange = (0, 3)
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
                { palCenterIndex = 5
                , palLength = 1
                , palText = "aba"
                , palRange = (10, 13)
                }
             )
           ]
        ~?= findPalindromes VarWord (ComQuadratic 0 0) (0, Nothing) "aba' bbb\n aba"

testFinderDNA =
    "testFinderDNA"
        ~: [ ( Palindrome
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
           ]
        ~?= findPalindromes VarDNA (ComQuadratic 0 0) (0, Nothing) "ATA"
