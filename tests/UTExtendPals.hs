{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}

module UTExtendPals (testListExtend) where

import Test.HUnit (Test (..), assertEqual)

import Data.Algorithms.Palindromes.Finders (Complexity)
import PalindromeMethods (extendTextPalindrome)

testExtendPalindrome1
    , testExtendPalindrome2
    , testExtendPalindrome3
    , testExtendPalindrome4
    , testExtendPalindrome5
    , testExtendPalindrome6
    , testExtendPalindrome7
    , testExtendPalindrome8
        :: Complexity -> Test

testListExtend t =
    [ TestLabel "testExtendPalindrome1" $ testExtendPalindrome1 t
    , TestLabel "testExtendPalindrome2" $ testExtendPalindrome2 t
    , TestLabel "testExtendPalindrome3" $ testExtendPalindrome3 t
    , TestLabel "testExtendPalindrome4" $ testExtendPalindrome4 t
    , TestLabel "testExtendPalindrome5" $ testExtendPalindrome5 t
    , TestLabel "testExtendPalindrome6" $ testExtendPalindrome6 t
    , TestLabel "testExtendPalindrome7" $ testExtendPalindrome7 t
    , TestLabel "testExtendPalindrome8" $ testExtendPalindrome8 t
    ]

testExtendPalindrome1 t =
    TestCase $
        assertEqual "extendPalindrome1" "e fe" $
            extendTextPalindrome t 39 "the cat jumped over the fence"

testExtendPalindrome2 t =
    TestCase $
        assertEqual "extendPalindrome2" "evil live" $
            extendTextPalindrome t 30 "They found an evil live broadcast"

testExtendPalindrome3 t =
    TestCase $
        assertEqual "extendPalindrome3" "o ho" $
            extendTextPalindrome t 31 "The coffee was too hot to drink"

testExtendPalindrome4 t =
    TestCase $
        assertEqual "extendPalindrome4" "d" $
            extendTextPalindrome t 3 "A dog barked in the distance"

testExtendPalindrome5 t =
    TestCase $
        assertEqual "extendPalindrome5" "oo" $
            extendTextPalindrome t 10 "The book fell off the shelf"

testExtendPalindrome6 t =
    TestCase $
        assertEqual "extendPalindrome6" "" $
            extendTextPalindrome t 0 "the cat jumped over the fence"

testExtendPalindrome7 t =
    TestCase $
        assertEqual "extendPalindrome7" "s s" $
            extendTextPalindrome t 10 "Birds sang outside my window"

testExtendPalindrome8 t =
    TestCase $
        assertEqual "extendPalindrome8" "racecar" $
            extendTextPalindrome t 13 "The racecar zoomed past us"
