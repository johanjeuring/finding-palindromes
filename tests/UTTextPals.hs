{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}

module UTTextPals (testListText) where

import Test.HUnit (Test (..), assertEqual, (~:), (~?=))

import Data.Algorithms.Palindromes.Finders (Algorithm)
import PalindromeMethods (longestTextPalindrome)

testListText :: Algorithm -> [Test]
testListText t =
    [ testTextPalindrome10 t
    , testTextPalindrome11 t
    , testTextPalindrome1 t
    , testTextPalindrome2 t
    , testTextPalindrome3 t
    , testTextPalindrome4 t
    , testTextPalindrome5 t
    , testTextPalindrome6 t
    , testTextPalindrome7 t
    , testTextPalindrome8 t
    , testTextPalindrome9 t
    ]

testTextPalindrome1 t = "testTextPalindrome1" ~: longestTextPalindrome t "abcdea,ba." ~?= "\"a,ba\""
testTextPalindrome2 t = "testTextPalindrome2" ~: longestTextPalindrome t "abcdea,ba" ~?= "\"a,ba\""
testTextPalindrome3 t = "testTextPalindrome3" ~: longestTextPalindrome t "abcde.a,ba" ~?= "\"a,ba\""
testTextPalindrome4 t = "testTextPalindrome4" ~: longestTextPalindrome t "abcde.a,baf" ~?= "\"a,ba\""
testTextPalindrome5 t = "testTextPalindrome5" ~: longestTextPalindrome t ".ab,acdef" ~?= "\"ab,a\""
testTextPalindrome6 t = "testTextPalindrome6" ~: longestTextPalindrome t "ab,acdef" ~?= "\"ab,a\""
testTextPalindrome7 t = "testTextPalindrome7" ~: longestTextPalindrome t "ab,a.cdef" ~?= "\"ab,a\""
testTextPalindrome8 t = "testTextPalindrome8" ~: longestTextPalindrome t "g.ab,a.cdef" ~?= "\"ab,a\""
testTextPalindrome9 t = "testTextPalindrome9" ~: longestTextPalindrome t "" ~?= "\"\""

testTextPalindrome10 t =
    TestCase $ do
        string <- readFile "./examples/palindromes/Damnitimmad.txt"
        assertEqual
            "testTextPalindrome10"
            ("\"" ++ init string ++ "\"")
            $ longestTextPalindrome t string

testTextPalindrome11 t =
    TestCase $ do
        string <- readFile "./examples/palindromes/pal17.txt"
        assertEqual
            "testTextPalindrome11"
            ("\"" ++ init string ++ "\"")
            $ longestTextPalindrome t string
