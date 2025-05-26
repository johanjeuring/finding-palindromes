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
    [ testITInsertionDeletion1
    , testITInsertionDeletion2
    , testITInsertionDeletion3
    , testITInsertionDeletion4
    , testITInsertionDeletion5
    , testITInsertionDeletion6
    , testITInsertionDeletion7
    , testITInsertionDeletion8
    , testITInsertionDeletion9
    , testITInsertionDeletion10
    , testITInsertionDeletion11
    , testITInsertionDeletion12
    , testITInsertionDeletion13
    , testITInsertionDeletion14
    , testITInsertionDeletion15
    , testITInsertionDeletion16
    , testITInsertionDeletion17
    , testITInsertionDeletion18
    , testITInsertionDeletion19
    , testITInsertionDeletion20
    , testITInsertionDeletion21
    , testITInsertionDeletion22
    , testITInsertionDeletion23
    , testITInsertionDeletion24
    , testITInsertionDeletion25
    , testITInsertionDeletion26
    , testITInsertionDeletion27
    , testITInsertionDeletion28
    , testITInsertionDeletion29
    , testITInsertionDeletion30
    , testITInsertionDeletion31
    , testITInsertionDeletion32
    , testITInsertionDeletion33
    , testITInsertionDeletion34
    , testITInsertionDeletion35
    , testITInsertionDeletion36
    , testITInsertionDeletion37
    , testITInsertionDeletion38
    , testITInsertionDeletion39
    , testITInsertionDeletion40
    , testITInsertionDeletion41
    , testITInsertionDeletion42
    , testITInsertionDeletion43
    , testITInsertionDeletion44
    , testITInsertionDeletion45
    , testITInsertionDeletion46
    , testITInsertionDeletion47
    , testITInsertionDeletion48
    , testITInsertionDeletion49
    , testITInsertionDeletion50
    , testITInsertionDeletion51
    , testITInsertionDeletion52
    , testITInsertionDeletion53
    , testITInsertionDeletion54
    , testITInsertionDeletion55
    , testITInsertionDeletion56
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
    , testITInsertionDeletion67
    , testITInsertionDeletion68
    , testITInsertionDeletion69
    , testITInsertionDeletion70
    , testITInsertionDeletion71
    , testITInsertionDeletion72
    , testITInsertionDeletion73
    , testITInsertionDeletion74
    ]

testITInsertionDeletion1 =
    "testITInsertionDeletion1"
        ~: findPalindromesFormatted
            VarDNA
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (0, Nothing)
            "AG\nTC"
        ~?= "3"

-- String: Contains a nested palindrome with punctuation
testITInsertionDeletion2 =
    "testITInsertionDeletion2"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (4, Just 4)
            "A&T-AT"
        ~?= "[\"A&T-AT\"]"

-- String: Does not contain a palindrome
testITInsertionDeletion3 =
    "testITInsertionDeletion3"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 4}
            (1, Just 6)
            "AGTC"
        ~?= "[4]"

-- String: Contains a gapped even palindrome
testITInsertionDeletion4 =
    "testITInsertionDeletion4"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            (2, Nothing)
            "ACCT"
        ~?= "[2,2,4]"

-- String: Contains an odd-gapped dna palindrome
testITInsertionDeletion5 =
    "testITInsertionDeletion5"
        ~: findPalindromesFormatted
            VarDNA
            OutWords
            ComInsertionDeletion{gapsID = 1, maxIDError = 2}
            (3, Just 6)
            "“AC\nTA..,TTCT”"
        ~?= "No palindromes found" -- "[\"ACTATTCT\"]" after max removed

-- String: Contains an even nested palindrome
testITInsertionDeletion6 =
    "testITInsertionDeletion6"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (3, Just 4)
            "ATAT"
        ~?= "[4]"

-- String: Contains an even gapped palindrome with punctuation
testITInsertionDeletion7 =
    "testITInsertionDeletion7"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComInsertionDeletion{gapsID = 4, maxIDError = 4}
            (2, Just 3)
            "Ac\nC,T”"
        ~?= "No palindromes found" -- "[\"Ac\nC,T”\"]" after max is removed

-- String: Contains an odd gapped palindrome
testITInsertionDeletion8 =
    "testITInsertionDeletion8"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComInsertionDeletion{gapsID = 1, maxIDError = 2}
            (2, Just 3)
            "ACTATTCT"
        ~?= "No palindromes found" -- "[\"ACTATTCT\"]" after max is removed

-- String: Contains an odd gapped palindrome
testITInsertionDeletion9 =
    "testITInsertionDeletion9"
        ~: findPalindromesFormatted
            VarDNA
            OutLength
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            (1, Just 6)
            "AGGGT"
        ~?= "2"

-- String: Contains an odd gapped palindrome with punctuation
testITInsertionDeletion10 =
    "testITInsertionDeletion10"
        ~: findPalindromesFormatted
            VarDNA
            OutWords
            ComInsertionDeletion{gapsID = 5, maxIDError = 5}
            (0, Nothing)
            "A;G;G;G;T"
        ~?= "[\"A;G;G;G;T\"]"

-- String: Contains a palindrome with punctuation
testITInsertionDeletion11 =
    "testITInsertionDeletion11"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (6, Just 6)
            "Ac.G-CgA "
        ~?= "[6]"

-- String: Contains an even palindrome
testITInsertionDeletion12 =
    "testITInsertionDeletion12"
        ~: findPalindromesFormatted
            VarDNA
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (6, Nothing)
            "ACGCGA"
        ~?= "No palindromes found"

-- String: Contains an approximate palindrome
testITInsertionDeletion13 =
    "testITInsertionDeletion13"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (4, Nothing)
            "kabral"
        ~?= "[\"abra\"]"

-- String: Contains no palindrome
testITInsertionDeletion14 =
    "testITInsertionDeletion14"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 6}
            (1, Just 2)
            "abcdef"
        ~?= "[6]"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion15 =
    "testITInsertionDeletion15"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            (3, Just 3)
            "a’b/ba"
        ~?= "6"

-- String: Contains an even palindrome
testITInsertionDeletion16 =
    "testITInsertionDeletion16"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (0, Nothing)
            "abba"
        ~?= "[\"abba\"]"

-- String: Contains an odd palindrome with punctuation. Contains a special character.
testITInsertionDeletion17 =
    "testITInsertionDeletion17"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (3, Just 6)
            "l.e.p’e;l"
        ~?= "[3]"

-- String: Contains an odd palindrome
testITInsertionDeletion18 =
    "testITInsertionDeletion18"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 5, maxIDError = 5}
            (2, Nothing)
            "lepel"
        ~?= "[\"lepel\"]"

-- String: Contains an approximate palindrome with punctuation
testITInsertionDeletion19 =
    "testITInsertionDeletion19"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (0, Nothing)
            "kab^ra.L"
        ~?= "5"

-- String: Contains no palindrome, contains punctuation
testITInsertionDeletion20 =
    "testITInsertionDeletion20"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (0, Just 0)
            "Abc'd/.ef"
        ~?= "[\"A\",\"b\",\"c\",\"'\",\"d\",\"/\",\".\",\"e\",\"f\"]"

-- String: Contains an odd approximate palindrome, with punctuation
testITInsertionDeletion21 =
    "testITInsertionDeletion21"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 10}
            (2, Nothing)
            "zat.,s&tat"
        ~?= "[10]"

-- String: Contains an odd approximate palindrome
testITInsertionDeletion22 =
    "testITInsertionDeletion22"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (2, Just 5)
            "zatstat"
        ~?= "[3,5]"

-- String: Contains an even gapped palindrome, with punctuation
testITInsertionDeletion23 =
    "testITInsertionDeletion23"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComInsertionDeletion{gapsID = 1, maxIDError = 2}
            (5, Just 8)
            "blaA\\Bc..dA;b.l#i"
        ~?= "[\"b.l#i\",\";b.l#\",\"A;b.l\",\"..dA;\",\"..dA;b.\",\"Bc..dA\",\"\\Bc..\",\"A\\Bc.\",\"aA\\Bc\",\"laA\\B\",\"blaA\\\"]"

-- String: Contains an even gapped palindrome
testITInsertionDeletion24 =
    "testITInsertionDeletion24"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            (1, Nothing)
            "blaABcdBAbli"
        ~?= "[2,2,2,2,2,2,2,6,2,2,2]"

-- String: Contains an odd gapped palindrome with punctuation
testITInsertionDeletion25 =
    "testITInsertionDeletion25"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 10, maxIDError = 10}
            (2, Nothing)
            "A-B*cde)BA"
        ~?= "[\"A-B*cde)BA\"]"

-- String: Contains an odd gapped palindrome
testITInsertionDeletion26 =
    "testITInsertionDeletion26"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 3, maxIDError = 2}
            (4, Just 7)
            "ABcdeBA"
        ~?= "[\"ABcdeBA\"]"

-- String: Contains an even palindrome, with punctuation, with special characters
testITInsertionDeletion27 :: Test
testITInsertionDeletion27 =
    "testITInsertionDeletion27"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (3, Just 6)
            "\"blaAPPab’li "
        ~?= "No palindromes found"

-- String: Contains an even palindrome
testITInsertionDeletion28 =
    "testITInsertionDeletion28"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 9}
            (0, Nothing)
            "blaAPPAbl"
        ~?= "[\"blaAPPAbl\"]"

-- String: Contains an odd palindrome
testITInsertionDeletion29 =
    "testITInsertionDeletion29"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (0, Nothing)
            "blaAPAbli"
        ~?= "[1,1,1,1,1,3,1,1,1]"

-- String: Contains an odd palindrome with punctuation
testITInsertionDeletion30 =
    "testITInsertionDeletion30"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (4, Just 7)
            "/blaAPa.bl.i"
        ~?= "[\".bl.\",\"aAPa\"]"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion31 =
    "testITInsertionDeletion31"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            (2, Just 5)
            "/abba/"
        ~?= "[\"abba\"]"

-- String: Conains an even gapped palindrome
testITInsertionDeletion32 =
    "testITInsertionDeletion32"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            (1, Just 6)
            ".,.ABcdBA,a"
        ~?= "6"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion33 =
    "testITInsertionDeletion33"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 5}
            (0, Nothing)
            "bla\\AP.PA.bli"
        ~?= "[\"bla\\AP.PA.bli\"]"

-- String: Contains an odd punctuation palindrome
testITInsertionDeletion34 =
    "testITInsertionDeletion34"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (3, Just 3)
            "bla(APA)bli"
        ~?= "[3,3,3]"

-- String: Contains an odd punctuation palindrome
testITInsertionDeletion35 =
    "testITInsertionDeletion35"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (2, Nothing)
            "lepel”"
        ~?= "5"

-- String: Contains an odd gapped palindrome with punctuation
testITInsertionDeletion36 =
    "testITInsertionDeletion36"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (2, Just 3)
            "AB/cde/BA"
        ~?= "No palindromes found"

-- String: Contains an approximate, even palindrome with punctuation
testITInsertionDeletion37 =
    "testITInsertionDeletion37"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (0, Nothing)
            "(ka(b)ral)"
        ~?= "3"

-- String: Contains an approximate odd palindrome
testITInsertionDeletion38 =
    "testITInsertionDeletion38"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (3, Just 3)
            "za.*ts&tat/”"
        ~?= "[\"tat\"]"

-- String: Contains no palindrome, with punctuation
testITInsertionDeletion39 =
    "testITInsertionDeletion39"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (2, Nothing)
            "Abc'd/.ef"
        ~?= "No palindromes found"

-- String: Contains no palindrome
testITInsertionDeletion40 =
    "testITInsertionDeletion40"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 3}
            (5, Just 5)
            "abcdef"
        ~?= "6"

-- String: Contains an even palindrome, contains punctuation and special characters
testITInsertionDeletion41 =
    "testITInsertionDeletion41"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (1, Nothing)
            "a’b/ba"
        ~?= "[\"a’b/ba\"]"

-- String: Contains an even palindrome
testITInsertionDeletion42 =
    "testITInsertionDeletion42"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (2, Just 8)
            "abba"
        ~?= "[\"abba\"]"

-- String: Contains an odd palindrome, contains punctuations and special characters
testITInsertionDeletion43 =
    "testITInsertionDeletion43"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (2, Nothing)
            "l.e.p’e;l"
        ~?= "[\"l.e.p’e;l\"]"

-- String: Contains an odd palindrome
testITInsertionDeletion44 =
    "testITInsertionDeletion44"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            (3, Just 6)
            "lepel"
        ~?= "[\"lepel\"]"

-- String: Contains an approximate even palindrome
testITInsertionDeletion45 =
    "testITInsertionDeletion45"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            (6, Just 6)
            "kab^ra.L"
        ~?= "6"

-- String: Contains an approximate even palindrome
testITInsertionDeletion46 =
    "testITInsertionDeletion46"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            (0, Nothing)
            "kabral"
        ~?= "[\"abra\"]"

-- String: Contains an approximate odd palindrome
testITInsertionDeletion47 =
    "testITInsertionDeletion47"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            1
            "zat.,s&tat"
        ~?= "[1,1,3,1,1,5,1]"

-- String: Contains an approximate odd palindrome
testITInsertionDeletion48 =
    "testITInsertionDeletion48"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 7}
            (2, Nothing)
            "zatstat"
        ~?= "[\"zatstat\"]"

-- String: Contains an even gapped palindrome with punctuation
testITInsertionDeletion49 =
    "testITInsertionDeletion49"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (0, Nothing)
            "blaA\\Bc..dA;b.l#i"
        ~?= "5"

-- String: Contains an even gapped palindrome
testITInsertionDeletion50 =
    "testITInsertionDeletion50"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (2, Just 2)
            "blaABcdBAbli"
        ~?= "[\"aA\"]"

-- String: Contains an odd gapped palindrome
testITInsertionDeletion51 =
    "testITInsertionDeletion51"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 8}
            (6, Nothing)
            "A-B*cde)BA"
        ~?= "[7]"

-- String: Contains an odd gapped palindrome
testITInsertionDeletion52 =
    "testITInsertionDeletion52"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComInsertionDeletion{gapsID = 3, maxIDError = 0}
            (2, Nothing)
            "ABcdeBA"
        ~?= "[3,3,3,3,7]"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion53 =
    "testITInsertionDeletion53"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            (3, Nothing)
            "\"blaAPPab'li"
        ~?= "[\"Pab'li\",\"blaAPP\",\"blaAPPab'li\"]"

-- String: Contains an even palindrome
testITInsertionDeletion54 =
    "testITInsertionDeletion54"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (1, Just 3)
            "blaAPPAbl"
        ~?= "[\"l\",\"b\",\"A\",\"aA\",\"l\",\"b\"]"
        ~?= "[\"b\",\"l\",\"aA\",\"APPA\",\"A\",\"b\",\"l\"]"

-- String: Contains an odd palindrome
testITInsertionDeletion55 =
    "testITInsertionDeletion55"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 9}
            (4, Just 7)
            ComInsertionDeletion{maxIDError = 9}
            4
            "blaAPAbli"
        ~?= "9"
        

-- String: Contains an odd palindrome with punctuation
testITInsertionDeletion56 =
    "testITInsertionDeletion56"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 3, maxIDError = 2}
            (4, Nothing)
            "/blaAPa.bl.i.bl"
        ~?= "[\"Pa.bl.i.bl\",\"APa.bl.i.b\",\"blaAPa.bl.i.b\"]"

-- String: Contains an odd palindrome with punctuation
testITInsertionDeletion56 =
    "testITInsertionDeletion56"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 3, maxIDError = 2}
            (4, Nothing)
            "/blaAPa.bl.i.bl"
        ~?= "[\"Pa.bl.i.bl\",\"APa.bl.i.b\",\"blaAPa.bl.i.b\"]"

-- String: Contains no palindromes, has punctuation
testITInsertionDeletion57 =
    "testITInsertionDeletion57"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (0, Nothing)
            "aba gdc."
        ~?= "2"

-- String: Contains no palindromes
testITInsertionDeletion58 =
    "testITInsertionDeletion58"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (0, Nothing)
            "aba gdc"
        ~?= "[1,1]"

-- String: Contains an even palindrome, has punctuation
testITInsertionDeletion59 =
    "testITInsertionDeletion59"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (2, Just 5)
            "Hello. hi hi “hello”"
        ~?= "[\"Hello. hi hi “hello\"]"

-- String: Contains an even palindrome
testITInsertionDeletion60 =
    "testITInsertionDeletion60"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            (6, Nothing)
            "hello hi hi hello"
        ~?= "No palindromes found"

-- String: Contains an odd palindrome, contains multiple spaces
testITInsertionDeletion61 =
    "testITInsertionDeletion61"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 3}
            (2, Nothing)
            "bye so    bye?"
        ~?= "[\"bye so    bye\"]"

-- String: Contains an odd palindrome
testITInsertionDeletion62 =
    "testITInsertionDeletion62"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 9}
            (3, Just 6)
            "bye so bye"
        ~?= "[\"bye so bye\"]"

-- String: Contains an approximate even palindrome with punctuation
testITInsertionDeletion63 =
    "testITInsertionDeletion63"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            (2, Just 7)
            "Fout. Weer. Hi. Hi. Niet. Goed."
        ~?= "[\"Fout. Weer. Hi. Hi. Niet. Goed\"]"

-- String: Contains an approximate even palindrome
testITInsertionDeletion64 =
    "testITInsertionDeletion64"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (0, Nothing)
            "fout weer hi hi niet goed"
        ~?= "4"

-- String: Contains odd approximate palindrome, has punctuation
testITInsertionDeletion65 =
    "testITInsertionDeletion65"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (2, Just 3)
            "Nope / fout / goed / niet / midden / oeps / goed / nee / ook"
        ~?= "No palindromes found"

-- String: Contains odd approximate palindrome
testITInsertionDeletion66 =
    "testITInsertionDeletion66"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 9}
            (4, Nothing)
            "nope fout goed niet midden oeps goed nee ook"
        ~?= "[9]"

-- String: Contains a gapped even palindrome with punctuation
testITInsertionDeletion67 =
    "testITInsertionDeletion67"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            (2, Just 3)
            "Doei&& hi ik b(e)n ??? een mens hi doei"
        ~?= "[2,2,2,2,2,2,2]"

-- String: Contains a gapped even palindrome
testITInsertionDeletion68 =
    "testITInsertionDeletion68"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 2, maxIDError = 1}
            (3, Just 8)
            "doei hi ik ben een mens hi doei"
        ~?= "[\"een mens hi doei\",\"ben een mens hi\",\"hi ik ben een\",\"doei hi ik ben\",\"doei hi ik ben een mens hi doei\"]"

-- String: Contains a gapped odd palindrome with punctuation
testITInsertionDeletion69 =
    "testITInsertionDeletion69"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 3, maxIDError = 1}
            (4, Just 9)
            "dag h?i dri/e gap size. ... hi dag"
        ~?= "[\"dag h?i dri/e gap size. ... hi dag\"]"

-- String: Contains a gapped odd palindrome
testITInsertionDeletion70 =
    "testITInsertionDeletion70"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{gapsID = 3, maxIDError = 0}
            (2, Just 8)
            "dag hi drie gap size hi dag"
        ~?= "7"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion71 =
    "testITInsertionDeletion71"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 6}
            (0, Nothing)
            ",onzin, .dit pal\n pal dit/ gek"
        ~?= "[\"onzin, .dit pal\n pal dit/ gek\"]"

-- String: Contains an even palindrome
testITInsertionDeletion72 =
    "testITInsertionDeletion72"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            (0, Nothing)
            "onzin dit pal pal dit gek"
        ~?= "[1,1,1,4,1]"

-- String: Contains an odd palindrome with punctuation
testITInsertionDeletion73 =
    "testITInsertionDeletion73"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            (2, Just 5)
            "Onzin. pAl is. Pal gek"
        ~?= "[\"Onzin. pAl is. Pal gek\"]"

-- String: Contains an odd palindrome
testITInsertionDeletion74 =
    "testITInsertionDeletion74"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            (3, Nothing)
            "onzin pal is pal gek"
        ~?= "[\"onzin pal is pal gek\"]"
