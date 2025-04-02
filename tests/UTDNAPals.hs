module UTDNAPals where

import PalindromeMethods (longestDNAPalindrome)
import Test.HUnit (Test (..), assertEqual)

testListDNA t =
    [ TestLabel "testDNAPalindrome1" $ testDNAPalindrome1 t
    , TestLabel "testDNAPalindrome2" $ testDNAPalindrome2 t
    ]

testDNAPalindrome1 t =
    TestCase $
        assertEqual "palindrome1" "gcgcgcatatatatgcgcgc" $
            longestDNAPalindrome t "gcgcgcatatatatgcgcgc"

testDNAPalindrome2 t =
    TestCase $
        assertEqual "palindrome1" "tatata" $
            longestDNAPalindrome t "tatata"
