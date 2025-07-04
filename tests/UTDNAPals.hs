{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring -}

module UTDNAPals (testListDNA) where

import Test.HUnit (Test (..), assertEqual)

import PalindromeMethods (longestDNAPalindrome)

testListDNA t =
    [ TestLabel "testDNAPalindrome1" $ testDNAPalindrome1 t
    , TestLabel "testDNAPalindrome2" $ testDNAPalindrome2 t
    ]

testDNAPalindrome1 t =
    TestCase $
        assertEqual "palindrome1" "\"gcgcgcatatatatgcgcgc\"" $
            longestDNAPalindrome t "gcgcgcatatatatgcgcgc"

testDNAPalindrome2 t =
    TestCase $
        assertEqual "palindrome1" "\"tatata\"" $
            longestDNAPalindrome t "tatata"
