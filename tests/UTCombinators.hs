module UTCombinators where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (ComQuadratic)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    , createCombinator
    )
import Data.Algorithms.Palindromes.PalindromesUtils
    ( DNA (A, C, G, T)
    , Palindrome (Palindrome, palCenterIndex, palLength, palRange, palText)
    , dnaToChar
    )
import Data.Char (isAlpha, toLower)
import Data.Vector (fromList)
import Test.HUnit (Test (..), assertEqual, (~:), (~?=))
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, forAll)

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