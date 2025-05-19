{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}

module ITInsertionDeletion (testListITInsertionDeletion) where

import Test.HUnit (Test (..), (~:), (~?=))

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    )

testListITInsertionDeletion =
    [ testITInsertionDeletion2
    , testITInsertionDeletion3
    , testITInsertionDeletion6
    , testITInsertionDeletion11
    , testITInsertionDeletion12
    , testITInsertionDeletion13
    , testITInsertionDeletion14
    , testITInsertionDeletion15
    , testITInsertionDeletion16
    , testITInsertionDeletion17
    , testITInsertionDeletion19
    , testITInsertionDeletion20
    , testITInsertionDeletion21
    , testITInsertionDeletion22
    , testITInsertionDeletion27
    , testITInsertionDeletion28
    , testITInsertionDeletion29
    , testITInsertionDeletion30
    , testITInsertionDeletion31
    , testITInsertionDeletion33
    , testITInsertionDeletion34
    , testITInsertionDeletion35
    , testITInsertionDeletion37
    , testITInsertionDeletion38
    , testITInsertionDeletion39
    , testITInsertionDeletion40
    , testITInsertionDeletion41
    , testITInsertionDeletion42
    , testITInsertionDeletion43
    , testITInsertionDeletion44
    , testITInsertionDeletion45
    , testITInsertionDeletion47
    , testITInsertionDeletion48
    , testITInsertionDeletion53
    , testITInsertionDeletion54
    , testITInsertionDeletion55
    , testITInsertionDeletion57
    , testITInsertionDeletion58
    , testITInsertionDeletion59
    , testITInsertionDeletion60
    , testITInsertionDeletion61
    , testITInsertionDeletion62
    , testITInsertionDeletion63
    , testITInsertionDeletion64
    , testITInsertionDeletion65
    , testITInsertionDeletion66
    , testITInsertionDeletion71
    , testITInsertionDeletion72
    , testITInsertionDeletion73
    , testITInsertionDeletion74
    ]

-- String: Contains a nested palindrome with punctuation
testITInsertionDeletion2 =
    "testITInsertionDeletion2"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComInsertionDeletion{maxIDError = 0}
            (4, Just 4)
            "A&T-AT"
        ~?= "A&T-AT"

-- String: Does not contain a palindrome
testITInsertionDeletion3 =
    "testITInsertionDeletion3"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{maxIDError = 4}
            (1, Just 6)
            "AGTC"
        ~?= "[4]"

-- String: Contains an even nested palindrome
testITInsertionDeletion6 =
    "testITInsertionDeletion6"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{maxIDError = 0}
            (3, Just 4)
            "ATAT"
        ~?= "[4]"

-- String: Contains a palindrome with punctuation
testITInsertionDeletion11 =
    "testITInsertionDeletion11"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{maxIDError = 1}
            (6, Just 6)
            "Ac.G-CgA "
        ~?= "[6]"

-- String: Contains an even palindrome
testITInsertionDeletion12 =
    "testITInsertionDeletion12"
        ~: findPalindromesFormatted
            VarDNA
            OutLength
            ComInsertionDeletion{maxIDError = 0}
            (6, Nothing)
            "ACGCGA"
        ~?= "No palindromes found"

-- String: Contains an approximate palindrome
testITInsertionDeletion13 =
    "testITInsertionDeletion13"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{maxIDError = 1}
            (4, Nothing)
            "kabral"
        ~?= "abra"

-- String: Contains no palindrome
testITInsertionDeletion14 =
    "testITInsertionDeletion14"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{maxIDError = 6}
            (1, Just 2)
            "abcdef"
        ~?= "No palindromes found"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion15 =
    "testITInsertionDeletion15"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComInsertionDeletion{maxIDError = 2}
            (3, Just 3)
            "a’b/ba"
        ~?= "No palindromes found"

-- String: Contains an even palindrome
testITInsertionDeletion16 =
    "testITInsertionDeletion16"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{maxIDError = 0}
            (0, Nothing)
            "abba"
        ~?= "abba"

-- String: Contains an odd palindrome with punctuation. Contains a special character.
testITInsertionDeletion17 =
    "testITInsertionDeletion17"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{maxIDError = 0}
            (3, Just 6)
            "l.e.p’e;l"
        ~?= "[3]"

-- String: Contains an approximate palindrome with punctuation
testITInsertionDeletion19 =
    "testITInsertionDeletion19"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComInsertionDeletion{maxIDError = 1}
            (0, Nothing)
            "kab^ra.L"
        ~?= "5"

-- String: Contains no palindrome, contains punctuation
testITInsertionDeletion20 =
    "testITInsertionDeletion20"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{maxIDError = 0}
            (0, Just 0)
            "Abc'd/.ef"
        ~?= "No palindromes found"

-- String: Contains an odd approximate palindrome, with punctuation
testITInsertionDeletion21 =
    "testITInsertionDeletion21"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{maxIDError = 10}
            (2, Nothing)
            "zat.,s&tat"
        ~?= "[10]"

-- String: Contains an odd approximate palindrome
testITInsertionDeletion22 =
    "testITInsertionDeletion22"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{maxIDError = 0}
            (2, Just 5)
            "zatstat"
        ~?= "[4,3]" -- should be "[5,3]"

-- String: Contains an even palindrome, with punctuation, with special characters
testITInsertionDeletion27 :: Test
testITInsertionDeletion27 =
    "testITInsertionDeletion27"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComInsertionDeletion{maxIDError = 0}
            (3, Just 6)
            "\"blaAPPab’li "
        ~?= "No palindromes found"

-- String: Contains an even palindrome
testITInsertionDeletion28 =
    "testITInsertionDeletion28"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComInsertionDeletion{maxIDError = 9}
            (0, Nothing)
            "blaAPPAbl"
        ~?= "[\"blaAPPAbl\"]"

-- String: Contains an odd palindrome
testITInsertionDeletion29 =
    "testITInsertionDeletion29"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{maxIDError = 0}
            (0, Nothing)
            "blaAPAbli"
        ~?= "[1,1,1,1,3,1,1,1,1]"

-- String: Contains an odd palindrome with punctuation
testITInsertionDeletion30 =
    "testITInsertionDeletion30"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComInsertionDeletion{maxIDError = 1}
            (4, Just 7)
            "/blaAPa.bl.i"
        ~?= "[\"aAPa\",\".bl.\"]"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion31 =
    "testITInsertionDeletion31"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWord
            ComInsertionDeletion{maxIDError = 2}
            (2, Just 5)
            "/abba/"
        ~?= "abba"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion33 =
    "testITInsertionDeletion33"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWords
            ComInsertionDeletion{maxIDError = 5}
            (0, Nothing)
            "bla\\AP.PA.bli"
        ~?= "[\"bla\\AP.PA.bli\"]"

-- String: Contains an odd punctuation palindrome
testITInsertionDeletion34 =
    "testITInsertionDeletion34"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLengths
            ComInsertionDeletion{maxIDError = 1}
            (3, Just 3)
            "bla(APA)bli"
        ~?= "[3,3,3]"

-- String: Contains an odd punctuation palindrome
testITInsertionDeletion35 =
    "testITInsertionDeletion35"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{maxIDError = 0}
            (2, Nothing)
            "lepel”"
        ~?= "5"

-- String: Contains an approximate, even palindrome with punctuation
testITInsertionDeletion37 =
    "testITInsertionDeletion37"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{maxIDError = 1}
            (0, Nothing)
            "(ka(b)ral)"
        ~?= "3"

-- String: Contains an approximate odd palindrome
testITInsertionDeletion38 =
    "testITInsertionDeletion38"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWord
            ComInsertionDeletion{maxIDError = 0}
            (3, Just 3)
            "za.*ts&tat/”"
        ~?= "tat"

-- String: Contains no palindrome, with punctuation
testITInsertionDeletion39 =
    "testITInsertionDeletion39"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComInsertionDeletion{maxIDError = 0}
            (2, Nothing)
            "Abc'd/.ef"
        ~?= "No palindromes found"

-- String: Contains no palindrome
testITInsertionDeletion40 =
    "testITInsertionDeletion40"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{maxIDError = 3}
            (5, Just 5)
            "abcdef"
        ~?= "No palindromes found"

-- String: Contains an even palindrome, contains punctuation and special characters
testITInsertionDeletion41 =
    "testITInsertionDeletion41"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{maxIDError = 1}
            (1, Nothing)
            "a’b/ba"
        ~?= "a’b/ba"

-- String: Contains an even palindrome
testITInsertionDeletion42 =
    "testITInsertionDeletion42"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{maxIDError = 0}
            (2, Just 8)
            "abba"
        ~?= "[\"abba\"]"

-- String: Contains an odd palindrome, contains punctuations and special characters
testITInsertionDeletion43 =
    "testITInsertionDeletion43"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{maxIDError = 1}
            (2, Nothing)
            "l.e.p’e;l"
        ~?= "l.e.p’e;l"

-- String: Contains an odd palindrome
testITInsertionDeletion44 =
    "testITInsertionDeletion44"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{maxIDError = 2}
            (3, Just 6)
            "lepel"
        ~?= "[\"lepel\"]"

-- String: Contains an approximate even palindrome
testITInsertionDeletion45 =
    "testITInsertionDeletion45"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{maxIDError = 2}
            (6, Just 6)
            "kab^ra.L"
        ~?= "6"

-- String: Contains an approximate odd palindrome
testITInsertionDeletion47 =
    "testITInsertionDeletion47"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComInsertionDeletion{maxIDError = 0}
            (1, Just 6)
            "zat.,s&tat"
        ~?= "[1,1,4,1,1,3,1]" -- should be "[1,1,1,5,1,1,1]"

-- String: Contains an approximate odd palindrome
testITInsertionDeletion48 =
    "testITInsertionDeletion48"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{maxIDError = 7}
            (2, Nothing)
            "zatstat"
        ~?= "zatstat"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion53 =
    "testITInsertionDeletion53"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{maxIDError = 2}
            (3, Nothing)
            "\"blaAPPab'li"
        ~?= "[\"blaAPP\",\"blaAPPab'li\",\"Pab'li\"]"

-- String: Contains an even palindrome
testITInsertionDeletion54 =
    "testITInsertionDeletion54"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{maxIDError = 0}
            (1, Just 3)
            "blaAPPAbl"
        ~?= "[\"b\",\"l\",\"aA\",\"A\",\"b\",\"l\"]"

-- String: Contains an odd palindrome
testITInsertionDeletion55 =
    "testITInsertionDeletion55"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{maxIDError = 9}
            (4, Just 7)
            "blaAPAbli"
        ~?= "No palindromes found"

-- String: Contains no palindromes, has punctuation
testITInsertionDeletion57 =
    "testITInsertionDeletion57"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{maxIDError = 1}
            (0, Nothing)
            "aba gdc."
        ~?= "2"

-- String: Contains no palindromes
testITInsertionDeletion58 =
    "testITInsertionDeletion58"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{maxIDError = 0}
            (0, Nothing)
            "aba gdc"
        ~?= "[1,1]"

-- String: Contains an even palindrome, has punctuation
testITInsertionDeletion59 =
    "testITInsertionDeletion59"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{maxIDError = 1}
            (2, Just 5)
            "Hello. hi hi “hello”"
        ~?= "[\"Hello. hi hi “hello\"]"

-- String: Contains an even palindrome
testITInsertionDeletion60 =
    "testITInsertionDeletion60"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{maxIDError = 2}
            (6, Nothing)
            "hello hi hi hello"
        ~?= "No palindromes found"

-- String: Contains an odd palindrome, contains multiple spaces
testITInsertionDeletion61 =
    "testITInsertionDeletion61"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{maxIDError = 3}
            (2, Nothing)
            "bye so    bye?"
        ~?= "[\"bye so    bye\"]"

-- String: Contains an odd palindrome
testITInsertionDeletion62 =
    "testITInsertionDeletion62"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{maxIDError = 9}
            (3, Just 6)
            "bye so bye"
        ~?= "bye so bye"

-- String: Contains an approximate even palindrome with punctuation
testITInsertionDeletion63 =
    "testITInsertionDeletion63"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{maxIDError = 2}
            (2, Just 7)
            "Fout. Weer. Hi. Hi. Niet. Goed."
        ~?= "Fout. Weer. Hi. Hi. Niet. Goed"

-- String: Contains an approximate even palindrome
testITInsertionDeletion64 =
    "testITInsertionDeletion64"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{maxIDError = 1}
            (0, Nothing)
            "fout weer hi hi niet goed"
        ~?= "4"

-- String: Contains odd approximate palindrome, has punctuation
testITInsertionDeletion65 =
    "testITInsertionDeletion65"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{maxIDError = 0}
            (2, Just 3)
            "Nope / fout / goed / niet / midden / oeps / goed / nee / ook"
        ~?= "No palindromes found"

-- String: Contains odd approximate palindrome
testITInsertionDeletion66 =
    "testITInsertionDeletion66"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{maxIDError = 9}
            (4, Nothing)
            "nope fout goed niet midden oeps goed nee ook"
        ~?= "[9]"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion71 =
    "testITInsertionDeletion71"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{maxIDError = 6}
            (0, Nothing)
            ",onzin, .dit pal\n pal dit/ gek"
        ~?= "[\"onzin, .dit pal\n pal dit/ gek\"]"

-- String: Contains an even palindrome
testITInsertionDeletion72 =
    "testITInsertionDeletion72"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{maxIDError = 0}
            (0, Nothing)
            "onzin dit pal pal dit gek"
        ~?= "[1,1,4,1,1]"

-- String: Contains an odd palindrome with punctuation
testITInsertionDeletion73 =
    "testITInsertionDeletion73"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{maxIDError = 1}
            (2, Just 5)
            "Onzin. pAl is. Pal gek"
        ~?= "[\"Onzin. pAl is. Pal gek\"]"

-- String: Contains an odd palindrome
testITInsertionDeletion74 =
    "testITInsertionDeletion74"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{maxIDError = 2}
            (3, Nothing)
            "onzin pal is pal gek"
        ~?= "onzin pal is pal gek"
