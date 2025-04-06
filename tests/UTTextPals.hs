module UTTextPals where

import Data.Algorithms.Palindromes.Finders (Complexity)
import PalindromeMethods (longestTextPalindrome)
import Test.HUnit (Test (..), assertEqual, (~:), (~?=))

testListText :: Complexity -> [Test]
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

testTextPalindrome1 t = "testTextPalindrome1" ~: longestTextPalindrome t "abcdea,ba." ~?= "a,ba"
testTextPalindrome2 t = "testTextPalindrome2" ~: longestTextPalindrome t "abcdea,ba" ~?= "a,ba"
testTextPalindrome3 t = "tesTextPalindrome3" ~: longestTextPalindrome t "abcde.a,ba" ~?= "a,ba"
testTextPalindrome4 t = "tesTextPalindrome4" ~: longestTextPalindrome t "abcde.a,baf" ~?= "a,ba"
testTextPalindrome5 t = "tesTextPalindrome5" ~: longestTextPalindrome t ".ab,acdef" ~?= "ab,a"
testTextPalindrome6 t = "tesTextPalindrome6" ~: longestTextPalindrome t "ab,acdef" ~?= "ab,a"
testTextPalindrome7 t = "tesTextPalindrome7" ~: longestTextPalindrome t "ab,a.cdef" ~?= "ab,a"
testTextPalindrome8 t = "tesTextPalindrome8" ~: longestTextPalindrome t "g.ab,a.cdef" ~?= "ab,a"
testTextPalindrome9 t = "tesTextPalindrome9" ~: longestTextPalindrome t "" ~?= ""

testTextPalindrome10 t =
    TestCase $ do
        string <- readFile "./examples/palindromes/Damnitimmad.txt"
        assertEqual
            "tesTextPalindrome10"
            (init string)
            $ longestTextPalindrome t string

testTextPalindrome11 t =
    TestCase $ do
        string <- readFile "./examples/palindromes/pal17.txt"
        assertEqual
            "tesTextPalindrome11"
            (init string)
            $ longestTextPalindrome t string
