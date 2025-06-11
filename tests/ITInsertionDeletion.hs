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
            0
            "AG\nTC"
        ~?= "3"

-- String: Contains a nested palindrome with punctuation
testITInsertionDeletion2 =
    "testITInsertionDeletion2"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            4
            "A&T-AT"
        ~?= "\"A&T-AT\""

-- String: Does not contain a palindrome
testITInsertionDeletion3 =
    "testITInsertionDeletion3"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 4}
            1
            "AGTC"
        ~?= "[4]"

-- String: Contains a gapped even palindrome
testITInsertionDeletion4 =
    "testITInsertionDeletion4"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            2
            "ACCT"
        ~?= "[2,2,4]"

-- String: Contains an odd-gapped dna palindrome
testITInsertionDeletion5 =
    "testITInsertionDeletion5"
        ~: findPalindromesFormatted
            VarDNA
            OutWords
            ComInsertionDeletion{gapsID = 1, maxIDError = 2}
            3
            "“AC\nTA..,TTCT”"
        ~?= "\"AC\nTA..,TTCT\""

-- String: Contains an even nested palindrome
testITInsertionDeletion6 =
    "testITInsertionDeletion6"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            3
            "ATAT"
        ~?= "[4]"

-- String: Contains an even gapped palindrome with punctuation
testITInsertionDeletion7 =
    "testITInsertionDeletion7"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComInsertionDeletion{gapsID = 4, maxIDError = 4}
            2
            "Ac\nC,T”"
        ~?= "\"Ac\nC,T\""

-- String: Contains an odd gapped palindrome
testITInsertionDeletion8 =
    "testITInsertionDeletion8"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComInsertionDeletion{gapsID = 1, maxIDError = 2}
            2
            "ACTATTCT"
        ~?= "\"ACTATTCT\""

-- String: Contains an odd gapped palindrome
testITInsertionDeletion9 =
    "testITInsertionDeletion9"
        ~: findPalindromesFormatted
            VarDNA
            OutLength
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            1
            "AGGGT"
        ~?= "2"

-- String: Contains an odd gapped palindrome with punctuation
testITInsertionDeletion10 =
    "testITInsertionDeletion10"
        ~: findPalindromesFormatted
            VarDNA
            OutWords
            ComInsertionDeletion{gapsID = 5, maxIDError = 5}
            0
            "A;G;G;G;T"
        ~?= "\"A;G;G;G;T\""

-- String: Contains a palindrome with punctuation
testITInsertionDeletion11 =
    "testITInsertionDeletion11"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            6
            "Ac.G-CgA "
        ~?= "[6]"

-- String: Contains an even palindrome
testITInsertionDeletion12 =
    "testITInsertionDeletion12"
        ~: findPalindromesFormatted
            VarDNA
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            6
            "ACGCGA"
        ~?= "No palindromes found"

-- String: Contains an approximate palindrome
testITInsertionDeletion13 =
    "testITInsertionDeletion13"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            4
            "kabral"
        ~?= "\"abra\""

-- String: Contains no palindrome
testITInsertionDeletion14 =
    "testITInsertionDeletion14"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 6}
            1
            "abcdef"
        ~?= "[6]"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion15 =
    "testITInsertionDeletion15"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            3
            "a’b/ba"
        ~?= "6"

-- String: Contains an even palindrome
testITInsertionDeletion16 =
    "testITInsertionDeletion16"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            0
            "abba"
        ~?= "\"abba\""

-- String: Contains an odd palindrome with punctuation. Contains a special character.
testITInsertionDeletion17 =
    "testITInsertionDeletion17"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            3
            "l.e.p’e;l"
        ~?= "[3]"

-- String: Contains an odd palindrome
testITInsertionDeletion18 =
    "testITInsertionDeletion18"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 5, maxIDError = 5}
            2
            "lepel"
        ~?= "\"lepel\""

-- String: Contains an approximate palindrome with punctuation
testITInsertionDeletion19 =
    "testITInsertionDeletion19"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            0
            "kab^ra.L"
        ~?= "5"

-- String: Contains no palindrome, contains punctuation
testITInsertionDeletion20 =
    "testITInsertionDeletion20"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            0
            "Abc'd/.ef"
        ~?= "\"f\"\n\"e\"\n\".\"\n\"/\"\n\"d\"\n\"'\"\n\"c\"\n\"b\"\n\"A\""

-- String: Contains an odd approximate palindrome, with punctuation
testITInsertionDeletion21 =
    "testITInsertionDeletion21"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 10}
            2
            "zat.,s&tat"
        ~?= "[10]"

-- String: Contains an odd approximate palindrome
testITInsertionDeletion22 =
    "testITInsertionDeletion22"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            2
            "zatstat"
        ~?= "[3,5]"

-- String: Contains an even gapped palindrome, with punctuation
testITInsertionDeletion23 =
    "testITInsertionDeletion23"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComInsertionDeletion{gapsID = 1, maxIDError = 2}
            5
            "blaA\\Bc..dA;b.l#i"
        ~?= "\"b.l#i\"\n\";b.l#\"\n\"A;b.l\"\n\"..dA;\"\n\"..dA;b.\"\n\"Bc..dA\"\n\"\\Bc..\"\n\"A\\Bc.\"\n\"aA\\Bc\"\n\"laA\\B\"\n\"blaA\\\""

-- String: Contains an even gapped palindrome
testITInsertionDeletion24 =
    "testITInsertionDeletion24"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            1
            "blaABcdBAbli"
        ~?= "[2,2,2,2,2,2,2,6,2,2,2]"

-- String: Contains an odd gapped palindrome with punctuation
testITInsertionDeletion25 =
    "testITInsertionDeletion25"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 10, maxIDError = 10}
            2
            "A-B*cde)BA"
        ~?= "\"A-B*cde)BA\""

-- String: Contains an odd gapped palindrome
testITInsertionDeletion26 =
    "testITInsertionDeletion26"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComInsertionDeletion{gapsID = 3, maxIDError = 2}
            4
            "ABcdeBA"
        ~?= "\"ABcdeBA\""

-- String: Contains an even palindrome, with punctuation, with special characters
testITInsertionDeletion27 :: Test
testITInsertionDeletion27 =
    "testITInsertionDeletion27"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            3
            "\"blaAPPab’li "
        ~?= "No palindromes found"

-- String: Contains an even palindrome
testITInsertionDeletion28 =
    "testITInsertionDeletion28"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 9}
            0
            "blaAPPAbl"
        ~?= "\"blaAPPAbl\""

-- String: Contains an odd palindrome
testITInsertionDeletion29 =
    "testITInsertionDeletion29"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            0
            "blaAPAbli"
        ~?= "[1,1,1,1,1,3,1,1,1]"

-- String: Contains an odd palindrome with punctuation
testITInsertionDeletion30 =
    "testITInsertionDeletion30"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            4
            "/blaAPa.bl.i"
        ~?= "\".bl.\"\n\"aAPa\""

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion31 =
    "testITInsertionDeletion31"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            2
            "/abba/"
        ~?= "\"abba\""

-- String: Conains an even gapped palindrome
testITInsertionDeletion32 =
    "testITInsertionDeletion32"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            1
            ".,.ABcdBA,a"
        ~?= "6"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion33 =
    "testITInsertionDeletion33"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 5}
            0
            "bla\\AP.PA.bli"
        ~?= "\"bla\\AP.PA.bli\""

-- String: Contains an odd punctuation palindrome
testITInsertionDeletion34 =
    "testITInsertionDeletion34"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            4
            "blaAPA)bli"
        ~?= "No palindromes found"

-- String: Contains an odd punctuation palindrome
testITInsertionDeletion35 =
    "testITInsertionDeletion35"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            2
            "lepel”"
        ~?= "5"

-- String: Contains an odd gapped palindrome with punctuation
testITInsertionDeletion36 =
    "testITInsertionDeletion36"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            2
            "AB/cde/BA"
        ~?= "No palindromes found"

-- String: Contains an approximate, even palindrome with punctuation
testITInsertionDeletion37 =
    "testITInsertionDeletion37"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            0
            "(ka(b)ral)"
        ~?= "3"

-- String: Contains an approximate odd palindrome
testITInsertionDeletion38 =
    "testITInsertionDeletion38"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            3
            "za.*ts&tat/”"
        ~?= "\"tat\""

-- String: Contains no palindrome, with punctuation
testITInsertionDeletion39 =
    "testITInsertionDeletion39"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            2
            "Abc'd/.ef"
        ~?= "No palindromes found"

-- String: Contains no palindrome
testITInsertionDeletion40 =
    "testITInsertionDeletion40"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 3}
            5
            "abcdef"
        ~?= "6"

-- String: Contains an even palindrome, contains punctuation and special characters
testITInsertionDeletion41 =
    "testITInsertionDeletion41"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            1
            "a’b/ba"
        ~?= "\"a’b/ba\""

-- String: Contains an even palindrome
testITInsertionDeletion42 =
    "testITInsertionDeletion42"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            2
            "abba"
        ~?= "\"abba\""

-- String: Contains an odd palindrome, contains punctuations and special characters
testITInsertionDeletion43 =
    "testITInsertionDeletion43"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            2
            "l.e.p’e;l"
        ~?= "\"l.e.p’e;l\""

-- String: Contains an odd palindrome
testITInsertionDeletion44 =
    "testITInsertionDeletion44"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            3
            "lepel"
        ~?= "\"lepel\""

-- String: Contains an approximate even palindrome
testITInsertionDeletion45 =
    "testITInsertionDeletion45"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            6
            "kab^ra.L"
        ~?= "6"

-- String: Contains an approximate even palindrome
testITInsertionDeletion46 =
    "testITInsertionDeletion46"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            0
            "kabral"
        ~?= "\"abra\""

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
            2
            "zatstat"
        ~?= "\"zatstat\""

-- String: Contains an even gapped palindrome with punctuation
testITInsertionDeletion49 =
    "testITInsertionDeletion49"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            0
            "blaA\\Bc..dA;b.l#i"
        ~?= "5"

-- String: Contains an even gapped palindrome
testITInsertionDeletion50 =
    "testITInsertionDeletion50"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            2
            "blaABcdBAbli"
        ~?= "\"BAb\""

-- String: Contains an odd gapped palindrome
testITInsertionDeletion51 =
    "testITInsertionDeletion51"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 8}
            6
            "A-B*cde)BA"
        ~?= "[7]"

-- String: Contains an odd gapped palindrome
testITInsertionDeletion52 =
    "testITInsertionDeletion52"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComInsertionDeletion{gapsID = 3, maxIDError = 0}
            2
            "ABcdeBA"
        ~?= "[3,3,3,3,7]"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion53 =
    "testITInsertionDeletion53"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            3
            "\"blaAPPab'li"
        ~?= "\"Pab'li\"\n\"blaAPP\"\n\"blaAPPab'li\""

-- String: Contains an even palindrome
testITInsertionDeletion54 =
    "testITInsertionDeletion54"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            1
            "blaAPPAbl"
        ~?= "\"l\"\n\"b\"\n\"A\"\n\"APPA\"\n\"aA\"\n\"l\"\n\"b\""

-- String: Contains an odd palindrome
testITInsertionDeletion55 =
    "testITInsertionDeletion55"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 9}
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
            4
            "/blaAPa.bl.i.bl"
        ~?= "\"Pa.bl.i.bl\"\n\"APa.bl.i.b\"\n\"blaAPa.bl.i.b\""

-- String: Contains no palindromes, has punctuation
testITInsertionDeletion57 =
    "testITInsertionDeletion57"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            0
            "aba gdc."
        ~?= "2"

-- String: Contains no palindromes
testITInsertionDeletion58 =
    "testITInsertionDeletion58"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            0
            "aba gdc"
        ~?= "[1,1]"

-- String: Contains an even palindrome, has punctuation
testITInsertionDeletion59 =
    "testITInsertionDeletion59"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            2
            "Hello. hi hi “hello”"
        ~?= "\"Hello. hi hi “hello\""

-- String: Contains an even palindrome
testITInsertionDeletion60 =
    "testITInsertionDeletion60"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            6
            "hello hi hi hello"
        ~?= "No palindromes found"

-- String: Contains an odd palindrome, contains multiple spaces
testITInsertionDeletion61 =
    "testITInsertionDeletion61"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 3}
            2
            "bye so    bye?"
        ~?= "\"bye so    bye\""

-- String: Contains an odd palindrome
testITInsertionDeletion62 =
    "testITInsertionDeletion62"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 9}
            3
            "bye so bye"
        ~?= "\"bye so bye\""

-- String: Contains an approximate even palindrome with punctuation
testITInsertionDeletion63 =
    "testITInsertionDeletion63"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            2
            "Fout. Weer. Hi. Hi. Niet. Goed."
        ~?= "\"Fout. Weer. Hi. Hi. Niet. Goed\""

-- String: Contains an approximate even palindrome
testITInsertionDeletion64 =
    "testITInsertionDeletion64"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            0
            "fout weer hi hi niet goed"
        ~?= "4"

-- String: Contains odd approximate palindrome, has punctuation
testITInsertionDeletion65 =
    "testITInsertionDeletion65"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            2
            "Nope / fout / goed / niet / midden / oeps / goed / nee / ook"
        ~?= "No palindromes found"

-- String: Contains odd approximate palindrome
testITInsertionDeletion66 =
    "testITInsertionDeletion66"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 9}
            4
            "nope fout goed niet midden oeps goed nee ook"
        ~?= "[9]"

-- String: Contains a gapped even palindrome with punctuation
testITInsertionDeletion67 =
    "testITInsertionDeletion67"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{gapsID = 2, maxIDError = 0}
            2
            "Doei&& hi ik b(e)n ??? een mens hi doei"
        ~?= "[2,2,2,2,2,2,2]"

-- String: Contains a gapped even palindrome
testITInsertionDeletion68 =
    "testITInsertionDeletion68"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 2, maxIDError = 1}
            3
            "doei hi ik ben een mens hi doei"
        ~?= "\"een mens hi doei\"\n\"ben een mens hi\"\n\"hi ik ben een\"\n\"doei hi ik ben\"\n\"doei hi ik ben een mens hi doei\""

-- String: Contains a gapped odd palindrome with punctuation
testITInsertionDeletion69 =
    "testITInsertionDeletion69"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 3, maxIDError = 1}
            4
            "dag h?i dri/e gap size. ... hi dag"
        ~?= "\"dag h?i dri/e gap size. ... hi dag\""

-- String: Contains a gapped odd palindrome
testITInsertionDeletion70 =
    "testITInsertionDeletion70"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            ComInsertionDeletion{gapsID = 3, maxIDError = 0}
            2
            "dag hi drie gap size hi dag"
        ~?= "7"

-- String: Contains an even palindrome with punctuation
testITInsertionDeletion71 =
    "testITInsertionDeletion71"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 6}
            0
            ",onzin, .dit pal\n pal dit/ gek"
        ~?= "\"onzin, .dit pal\n pal dit/ gek\""

-- String: Contains an even palindrome
testITInsertionDeletion72 =
    "testITInsertionDeletion72"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            ComInsertionDeletion{gapsID = 0, maxIDError = 0}
            0
            "onzin dit pal pal dit gek"
        ~?= "[1,1,1,4,1]"

-- String: Contains an odd palindrome with punctuation
testITInsertionDeletion73 =
    "testITInsertionDeletion73"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            ComInsertionDeletion{gapsID = 0, maxIDError = 1}
            2
            "Onzin. pAl is. Pal gek"
        ~?= "\"Onzin. pAl is. Pal gek\""

-- String: Contains an odd palindrome
testITInsertionDeletion74 =
    "testITInsertionDeletion74"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            ComInsertionDeletion{gapsID = 0, maxIDError = 2}
            3
            "onzin pal is pal gek"
        ~?= "\"onzin pal is pal gek\""
