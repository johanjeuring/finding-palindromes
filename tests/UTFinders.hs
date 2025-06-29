{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring -}

module UTFinders (testListFinders) where

import Test.HUnit (Test (..), (~:), (~?=))

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (AlgQuadratic)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    , findPalindromes
    )
import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (..)
    )

testListFinders =
    [ testFinderPlain
    , testFinderText
    , testFinderWord
    , testFinderDNA
    , testFinderDNA
    ]

testFinderPlain =
    "testFinderPlain"
        ~: findPalindromes VarPlain (AlgQuadratic 0 0) 1 "aba"
        ~?= [ ( Palindrome
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
                    { palRange = (2, 3)
                    , palText = "a"
                    , palRangeInText = (2, 3)
                    }
              )
            ]

testFinderText =
    "testFinderText"
        ~: findPalindromes VarText (AlgQuadratic 0 0) 1 "ab'A"
        ~?= [ ( Palindrome
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

testFinderWord =
    "testFinderWord"
        ~: findPalindromes VarWord (AlgQuadratic 0 0) 1 "aba' bbb\n aba"
        ~?= [ ( Palindrome
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

testFinderDNA =
    "testFinderDNA"
        ~: findPalindromes VarDNA (AlgQuadratic 0 0) 1 "ATA"
        ~?= [ ( Palindrome
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
