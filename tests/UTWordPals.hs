{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}

module UTWordPals where

import Test.HUnit (Test (..), (~:), (~?=))

import PalindromeMethods (longestWordPalindrome)

testListWordPalindromes t =
    [ testWord1 t
    , testWord2 t
    , testWord3 t
    , testWord4 t
    , testWord5 t
    , testWord6 t
    , testWord7 t
    , testWord8 t
    , testWord9 t
    , testWord10 t
    , testWord11 t
    , testWord12 t
    ]

testWord1 t =
    "testWord1"
        ~: longestWordPalindrome
            t
            "abc def def abc"
        ~?= "abc def def abc"

testWord2 t =
    "testWord2"
        ~: longestWordPalindrome
            t
            "abc abc def abcdefghi"
        ~?= "abc abc"

testWord3 t =
    "testWord3"
        ~: longestWordPalindrome
            t
            "AbC aBb a bb Abc aBC"
        ~?= "Abc aBC"

testWord4 t =
    "testWord4"
        ~: longestWordPalindrome
            t
            "abc def     abc"
        ~?= "abc def     abc"

testWord5 t =
    "testWord5"
        ~: longestWordPalindrome
            t
            "abc., abc"
        ~?= "abc., abc"

testWord6 t =
    "testWord6"
        ~: longestWordPalindrome
            t
            "abc.d   abcd .. . a"
        ~?= "abc.d   abcd"

testWord7 t =
    "testWord7"
        ~: longestWordPalindrome
            t
            "a a"
        ~?= "a a"

testWord8 t =
    "testWord8"
        ~: longestWordPalindrome
            t
            ""
        ~?= ""
testWord9 t =
    "testWord9"
        ~: longestWordPalindrome
            t
            " leading spaces"
        ~?= "leading"

testWord10 t =
    "testWord10"
        ~: longestWordPalindrome
            t
            "/.;/;,';,.,,leading garbage"
        ~?= "leading"

testWord11 t =
    "testWord11"
        ~: longestWordPalindrome
            t
            "several; different,.; characters .char-acter[s"
        ~?= "characters .char-acter[s"

testWord12 t =
    "testWord12"
        ~: longestWordPalindrome
            t
            "."
        ~?= ""
