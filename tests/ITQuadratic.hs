{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}

module ITQuadratic where

import Test.HUnit (Test (..), (~:), (~?=))

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    )

testListITQuadratic =
    [ testITQuadratic1
    , testITQuadratic2
    , testITQuadratic3
    , testITQuadratic4
    , testITQuadratic5
    , testITQuadratic6
    , testITQuadratic7
    , testITQuadratic8
    , testITQuadratic9
    , testITQuadratic10
    , testITQuadratic11
    , testITQuadratic12
    , testITQuadratic13
    , testITQuadratic14
    , testITQuadratic15
    , testITQuadratic16
    , testITQuadratic17
    , testITQuadratic18
    , testITQuadratic19
    , testITQuadratic20
    , testITQuadratic21
    , testITQuadratic22
    , testITQuadratic23
    , testITQuadratic24
    , testITQuadratic25
    , testITQuadratic26
    , testITQuadratic27
    , testITQuadratic28
    , testITQuadratic29
    , testITQuadratic30
    , testITQuadratic31
    , testITQuadratic32
    , testITQuadratic33
    , testITQuadratic34
    , testITQuadratic35
    , testITQuadratic36
    , testITQuadratic37
    , testITQuadratic38
    , testITQuadratic39
    , testITQuadratic40
    , testITQuadratic41
    , testITQuadratic42
    , testITQuadratic43
    , testITQuadratic44
    , testITQuadratic45
    , testITQuadratic46
    , testITQuadratic47
    , testITQuadratic48
    , testITQuadratic49
    , testITQuadratic50
    , testITQuadratic51
    , testITQuadratic52
    , testITQuadratic53
    , testITQuadratic54
    , testITQuadratic55
    , testITQuadratic56
    , testITQuadratic57
    , testITQuadratic58
    , testITQuadratic59
    , testITQuadratic60
    , testITQuadratic61
    , testITQuadratic62
    , testITQuadratic63
    , testITQuadratic64
    , testITQuadratic65
    , testITQuadratic66
    , testITQuadratic67
    , testITQuadratic68
    , testITQuadratic69
    , testITQuadratic70
    , testITQuadratic71
    , testITQuadratic72
    , testITQuadratic73
    , testITQuadratic74
    ]

-- String: Contains a gapped palindrome with punctuation
testITQuadratic1 =
    "testITQuadratic1"
        ~: findPalindromesFormatted
            VarDNA
            OutLength
            ComQuadratic{gapSize = 0, maxError = 1}
            (0, Nothing)
            "AG\nTC"
        ~?= "2"

-- String: Contains a nested palindrome with punctuation
testITQuadratic2 =
    "testITQuadratic2"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComQuadratic{gapSize = 0, maxError = 0}
            (4, Just 4)
            "A&T-AT"
        ~?= "A&T-AT"

-- String: Does not contain a palindrome
testITQuadratic3 =
    "testITQuadratic3"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComQuadratic{gapSize = 4, maxError = 4}
            (1, Just 6)
            "AGTC"
        ~?= "[0,2,4,2,0]"

-- String: Contains a gapped even palindrome
testITQuadratic4 =
    "testITQuadratic4"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComQuadratic{gapSize = 2, maxError = 0}
            (2, Nothing)
            "ACCT"
        ~?= "[0,2,4,2,0]"

-- String: Contains an odd-gapped dna palindrome
testITQuadratic5 =
    "testITQuadratic5"
        ~: findPalindromesFormatted
            VarDNA
            OutWords
            ComQuadratic{gapSize = 1, maxError = 2}
            (3, Just 6)
            "“AC\nTA..,TTCT”"
        ~?= "[\"\",\"\",\"\",\"AC\nT\",\"AC\nTA\",\"AC\nTA..,T\",\"AC\nTA..,TT\",\"C\nTA..,TT\",\"\",\"\",\"A..,TTC\",\"A..,TTCT\",\"TTCT\",\"TCT\",\"\",\"\",\"\"]"

-- String: Contains an even nested palindrome
testITQuadratic6 =
    "testITQuadratic6"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComQuadratic{gapSize = 0, maxError = 0}
            (3, Just 4)
            "ATAT"
        ~?= "[0,0,4,0,0]"

-- String: Contains an even gapped palindrome with punctuation
testITQuadratic7 =
    "testITQuadratic7"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComQuadratic{gapSize = 4, maxError = 4}
            (2, Just 3)
            "Ac\nC,T”"
        ~?= "Ac"

-- String: Contains an odd gapped palindrome
testITQuadratic8 =
    "testITQuadratic8"
        ~: findPalindromesFormatted
            VarDNA
            OutWord
            ComQuadratic{gapSize = 1, maxError = 2}
            (2, Just 3)
            "ACTATTCT"
        ~?= "ACT"

-- String: Contains an odd gapped palindrome
testITQuadratic9 =
    "testITQuadratic9"
        ~: findPalindromesFormatted
            VarDNA
            OutLength
            ComQuadratic{gapSize = 2, maxError = 0}
            (1, Just 6)
            "AGGGT"
        ~?= "2"

-- String: Contains an odd gapped palindrome with punctuation
testITQuadratic10 =
    "testITQuadratic10"
        ~: findPalindromesFormatted
            VarDNA
            OutWords
            ComQuadratic{gapSize = 5, maxError = 5}
            (0, Nothing)
            "A;G;G;G;T"
        ~?= "[\"\",\"A\",\"A;G\",\"A;G;G\",\"A;G;G;G\",\"A;G;G;G;T\",\"G;G;G;T\",\"G;G;T\",\"G;T\",\"T\",\"\"]"

-- String: Contains a palindrome with punctuation
testITQuadratic11 =
    "testITQuadratic11"
        ~: findPalindromesFormatted
            VarDNA
            OutLengths
            ComQuadratic{gapSize = 0, maxError = 1}
            (6, Just 6)
            "Ac.G-CgA "
        ~?= "[0,0,0,6,0,0,0]"

-- String: Contains an even palindrome
testITQuadratic12 =
    "testITQuadratic12"
        ~: findPalindromesFormatted
            VarDNA
            OutLength
            ComQuadratic{gapSize = 0, maxError = 0}
            (6, Nothing)
            "ACGCGA"
        ~?= "0"

-- String: Contains an approximate palindrome
testITQuadratic13 =
    "testITQuadratic13"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComQuadratic{gapSize = 0, maxError = 1}
            (4, Nothing)
            "kabral"
        ~?= "abra"

-- String: Contains no palindrome
testITQuadratic14 =
    "testITQuadratic14"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComQuadratic{gapSize = 6, maxError = 6}
            (1, Just 2)
            "abcdef"
        ~?= "[0,1,2,0,0,0,0,0,0,0,2,1,0]"

-- String: Contains an even palindrome with punctuation
testITQuadratic15 =
    "testITQuadratic15"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComQuadratic{gapSize = 1, maxError = 2}
            (3, Just 3)
            "a’b/ba"
        ~?= "3"

-- String: Contains an even palindrome
testITQuadratic16 =
    "testITQuadratic16"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComQuadratic{gapSize = 2, maxError = 0}
            (0, Nothing)
            "abba"
        ~?= "abba"

-- String: Contains an odd palindrome with punctuation. Contains a special character.
testITQuadratic17 =
    "testITQuadratic17"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComQuadratic{gapSize = 0, maxError = 0}
            (3, Just 6)
            "l.e.p’e;l"
        ~?= "[0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0]"

-- String: Contains an odd palindrome
testITQuadratic18 =
    "testITQuadratic18"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComQuadratic{gapSize = 5, maxError = 5}
            (2, Just 3)
            "lepel"
        ~?= "lep"

-- String: Contains an approximate palindrome with punctuation
testITQuadratic19 =
    "testITQuadratic19"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComQuadratic{gapSize = 0, maxError = 1}
            (0, Nothing)
            "kab^ra.L"
        ~?= "5"

-- String: Contains no palindrome, contains punctuation
testITQuadratic20 =
    "testITQuadratic20"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComQuadratic{gapSize = 0, maxError = 0}
            (0, Just 0)
            "Abc'd/.ef"
        ~?= ""

-- String: Contains an odd approximate palindrome, with punctuation
testITQuadratic21 =
    "testITQuadratic21"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComQuadratic{gapSize = 10, maxError = 10}
            (2, Nothing)
            "zat.,s&tat"
        ~?= "[0,0,2,3,4,5,6,7,8,9,10,9,8,7,6,5,4,3,2,0,0]"

-- String: Contains an odd approximate palindrome
testITQuadratic22 =
    "testITQuadratic22"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComQuadratic{gapSize = 2, maxError = 0}
            (2, Just 5)
            "zatstat"
        ~?= "[0,0,2,0,2,0,2,5,2,0,2,3,2,0,0]"

-- String: Contains an even gapped palindrome, with punctuation
testITQuadratic23 =
    "testITQuadratic23"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComQuadratic{gapSize = 1, maxError = 2}
            (5, Just 8)
            "blaA\\Bc..dA;b.l#i"
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"blaA\\\",\"\",\"laA\\B\",\"\",\"aA\\Bc\",\"\",\"A\\Bc.\",\"\",\"\\Bc..\",\"\",\"Bc..d\",\"Bc..dA\",\"c..dA\",\"\",\"..dA;\",\"\",\"..dA;b.\",\".dA;b.\",\"dA;b.\",\"\",\"A;b.l\",\"\",\";b.l#\",\"\",\"b.l#i\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Contains an even gapped palindrome
testITQuadratic24 =
    "testITQuadratic24"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComQuadratic{gapSize = 0, maxError = 0}
            (1, Just 4)
            "blaABcdBAbli"
        ~?= "[0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0]"

-- String: Contains an odd gapped palindrome with punctuation
testITQuadratic25 =
    "testITQuadratic25"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComQuadratic{gapSize = 10, maxError = 10}
            (2, Just 5)
            "A-B*cde)BA"
        ~?= "A-B*c"

-- String: Contains an odd gapped palindrome
testITQuadratic26 =
    "testITQuadratic26"
        ~: findPalindromesFormatted
            VarPlain
            OutWord
            ComQuadratic{gapSize = 3, maxError = 2}
            (4, Just 7)
            "ABcdeBA"
        ~?= "ABcdeBA"

-- String: Contains an even palindrome, with punctuation, with special characters
testITQuadratic27 =
    "testITQuadratic27"
        ~: findPalindromesFormatted
            VarPlain
            OutLength
            ComQuadratic{gapSize = 2, maxError = 0}
            (3, Just 6)
            "\"blaAPPab’li "
        ~?= "0"

-- String: Contains an even palindrome
testITQuadratic28 =
    "testITQuadratic28"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComQuadratic{gapSize = 9, maxError = 9}
            (0, Nothing)
            "blaAPPAbl"
        ~?= "[\"\",\"b\",\"bl\",\"bla\",\"blaA\",\"blaAP\",\"blaAPP\",\"blaAPPA\",\"blaAPPAb\",\"blaAPPAbl\",\"laAPPAbl\",\"aAPPAbl\",\"APPAbl\",\"PPAbl\",\"PAbl\",\"Abl\",\"bl\",\"l\",\"\"]"

-- String: Contains an odd palindrome
testITQuadratic29 =
    "testITQuadratic29"
        ~: findPalindromesFormatted
            VarPlain
            OutLengths
            ComQuadratic{gapSize = 0, maxError = 0}
            (0, Nothing)
            "blaAPAbli"
        ~?= "[0,1,0,1,0,1,0,1,0,3,0,1,0,1,0,1,0,1,0]"

-- String: Contains an odd palindrome with punctuation
testITQuadratic30 =
    "testITQuadratic30"
        ~: findPalindromesFormatted
            VarPlain
            OutWords
            ComQuadratic{gapSize = 0, maxError = 1}
            (4, Just 7)
            "/blaAPa.bl.i"
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"aAPa\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\".bl.\",\"\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Contains an even palindrome with punctuation
testITQuadratic31 =
    "testITQuadratic31"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWord
            ComQuadratic{gapSize = 3, maxError = 2}
            (2, Just 5)
            "/abba/"
        ~?= "abba"

-- String: Conains an even gapped palindrome
testITQuadratic32 =
    "testITQuadratic32"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComQuadratic{gapSize = 2, maxError = 0}
            (1, Just 6)
            "ABcdBA"
        ~?= "6"

-- String: Contains an even palindrome with punctuation
testITQuadratic33 =
    "testITQuadratic33"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWords
            ComQuadratic{gapSize = 10, maxError = 5}
            (0, Nothing)
            "bla\\AP.PA.bli"
        ~?= "[\"\",\"\",\"\",\"bla\",\"\",\"bla\\AP\",\"\",\"bla\\AP.PA\",\"AP\",\"\",\"bla\\AP.PA.bli\",\"\",\"PA\",\"AP.PA.bli\",\"\",\"PA.bli\",\"\",\"bli\",\"\",\"\",\"\"]"

-- String: Contains an odd punctuation palindrome
testITQuadratic34 =
    "testITQuadratic34"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLengths
            ComQuadratic{gapSize = 0, maxError = 1}
            (3, Just 3)
            "bla(APA)bli"
        ~?= "[0,0,0,3,0,0,0,0,0,3,0,0,0,0,0,3,0,0,0]"

-- String: Contains an odd punctuation palindrome
testITQuadratic35 =
    "testITQuadratic35"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComQuadratic{gapSize = 0, maxError = 0}
            (2, Nothing)
            "lepel”"
        ~?= "5"

-- String: Contains an odd gapped palindrome with punctuation
testITQuadratic36 =
    "testITQuadratic36"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComQuadratic{gapSize = 0, maxError = 0}
            (2, Just 3)
            "AB/cde/BA"
        ~?= "0"

-- String: Contains an approximate, even palindrome with punctuation
testITQuadratic37 =
    "testITQuadratic37"
        ~: findPalindromesFormatted
            VarPunctuation
            OutLength
            ComQuadratic{gapSize = 0, maxError = 1}
            (0, Nothing)
            "(ka(b)ral)"
        ~?= "3"

-- String: Contains an approximate odd palindrome
testITQuadratic38 =
    "testITQuadratic38"
        ~: findPalindromesFormatted
            VarPunctuation
            OutWord
            ComQuadratic{gapSize = 0, maxError = 0}
            (3, Just 3)
            "za.*ts&tat/”"
        ~?= "tat"

-- String: Contains no palindrome, with punctuation
testITQuadratic39 =
    "testITQuadratic39"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComQuadratic{gapSize = 2, maxError = 0}
            (2, Nothing)
            "Abc'd/.ef"
        ~?= "[0,0,2,0,2,0,2,0,2,0,2,0,0]"

-- String: Contains no palindrome
testITQuadratic40 =
    "testITQuadratic40"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComQuadratic{gapSize = 6, maxError = 3}
            (5, Just 5)
            "abcdef"
        ~?= "5"

-- String: Contains an even palindrome, contains punctuation and special characters
testITQuadratic41 =
    "testITQuadratic41"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComQuadratic{gapSize = 0, maxError = 1}
            (1, Just 2)
            "a’b/ba"
        ~?= "a’b"

-- String: Contains an even palindrome
testITQuadratic42 =
    "testITQuadratic42"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComQuadratic{gapSize = 0, maxError = 0}
            (2, Just 8)
            "abba"
        ~?= "[\"\",\"\",\"\",\"\",\"abba\",\"\",\"\",\"\",\"\"]"

-- String: Contains an odd palindrome, contains punctuations and special characters
testITQuadratic43 =
    "testITQuadratic43"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComQuadratic{gapSize = 0, maxError = 1}
            (2, Nothing)
            "l.e.p’e;l"
        ~?= "l.e.p’e;l"

-- String: Contains an odd palindrome
testITQuadratic44 =
    "testITQuadratic44"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComQuadratic{gapSize = 5, maxError = 2}
            (3, Just 6)
            "lepel"
        ~?= "[\"\",\"\",\"\",\"lep\",\"lepe\",\"lepel\",\"epel\",\"pel\",\"\",\"\",\"\"]"

-- String: Contains an approximate even palindrome
testITQuadratic45 =
    "testITQuadratic45"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComQuadratic{gapSize = 1, maxError = 2}
            (5, Just 5)
            "kab^ra.L"
        ~?= "5"

-- String: Contains an approximate even palindrome
testITQuadratic46 =
    "testITQuadratic46"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComQuadratic{gapSize = 2, maxError = 0}
            (0, Nothing)
            "kabral"
        ~?= "abra"

-- String: Contains an approximate odd palindrome
testITQuadratic47 =
    "testITQuadratic47"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComQuadratic{gapSize = 0, maxError = 0}
            (1, Just 6)
            "zat.,s&tat"
        ~?= "[0,1,0,1,0,1,0,5,0,1,0,3,0,1,0]"

-- String: Contains an approximate odd palindrome
testITQuadratic48 =
    "testITQuadratic48"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComQuadratic{gapSize = 7, maxError = 7}
            (2, Just 5)
            "zatstat"
        ~?= "zatst"

-- String: Contains an even gapped palindrome with punctuation
testITQuadratic49 =
    "testITQuadratic49"
        ~: findPalindromesFormatted
            VarText
            OutLength
            ComQuadratic{gapSize = 0, maxError = 1}
            (0, Nothing)
            "blaA\\Bc..dA;b.l#i"
        ~?= "5"

-- String: Contains an even gapped palindrome
testITQuadratic50 =
    "testITQuadratic50"
        ~: findPalindromesFormatted
            VarText
            OutWord
            ComQuadratic{gapSize = 0, maxError = 0}
            (2, Just 2)
            "blaABcdBAbli"
        ~?= "aA"

-- String: Contains an odd gapped palindrome
testITQuadratic51 =
    "testITQuadratic51"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComQuadratic{gapSize = 0, maxError = 8}
            (6, Nothing)
            "A-B*cde)BA"
        ~?= "[0,0,0,0,0,0,6,7,6,0,0,0,0,0,0]"

-- String: Contains an odd gapped palindrome
testITQuadratic52 =
    "testITQuadratic52"
        ~: findPalindromesFormatted
            VarText
            OutLengths
            ComQuadratic{gapSize = 3, maxError = 0}
            (2, Just 5)
            "ABcdeBA"
        ~?= "[0,0,2,3,2,3,2,0,2,3,2,3,2,0,0]"

-- String: Contains an even palindrome with punctuation
testITQuadratic53 =
    "testITQuadratic53"
        ~: findPalindromesFormatted
            VarText
            OutWords
            ComQuadratic{gapSize = 1, maxError = 2}
            (3, Just 6)
            "\"blaAPPab'li"
        ~?= "[\"\",\"\",\"\",\"bla\",\"blaA\",\"blaAP\",\"blaAPP\",\"laAPP\",\"aAPP\",\"\",\"\",\"APPab\",\"PPab\",\"PPab'l\",\"Pab'l\",\"Pab'li\",\"ab'li\",\"b'li\",\"\",\"\",\"\"]"

-- String: Contains an even palindrome
testITQuadratic54 =
    "testITQuadratic54"
        ~: findPalindromesFormatted
            VarText
            OutWords
            (ComQuadratic 0 0)
            (1, Just 3)
            "blaAPPAbl"
        ~?= "[\"\",\"b\",\"\",\"l\",\"\",\"a\",\"aA\",\"A\",\"\",\"P\",\"\",\"P\",\"\",\"A\",\"\",\"b\",\"\",\"l\",\"\"]"

-- String: Contains an odd palindrome
testITQuadratic55 =
    "testITQuadratic55"
        ~: findPalindromesFormatted
            VarText
            OutLength
            (ComQuadratic 9 9)
            (4, Just 7)
            "blaAPAbli"
        ~?= "7"

-- String: Contains an odd palindrome with punctuation
testITQuadratic56 =
    "testITQuadratic56"
        ~: findPalindromesFormatted
            VarText
            OutWord
            (ComQuadratic 3 2)
            (4, Just 7)
            "/blaAPa.bl.i.bl"
        ~?= "blaAPa.b"

-- String: Contains no palindromes, has punctuation
testITQuadratic57 =
    "testITQuadratic57"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            (ComQuadratic 0 1)
            (0, Nothing)
            "aba gdc."
        ~?= "2"

-- String: Contains no palindromes
testITQuadratic58 =
    "testITQuadratic58"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            (ComQuadratic 0 0)
            (0, Nothing)
            "aba gdc"
        ~?= "[0,1,0,1,0]"

-- String: Contains an even palindrome, has punctuation
testITQuadratic59 =
    "testITQuadratic59"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            (ComQuadratic 0 1)
            (2, Just 5)
            "Hello. hi hi “hello”"
        ~?= "[\"\",\"\",\"Hello. hi\",\"Hello. hi hi\",\"Hello. hi hi “hello\",\"hi hi “hello\",\"hi “hello\",\"\",\"\"]"

-- String: Contains an even palindrome
testITQuadratic60 =
    "testITQuadratic60"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            (ComQuadratic 1 2)
            (6, Nothing)
            "hello hi hi hello"
        ~?= "0"

-- String: Contains an odd palindrome, contains multiple spaces
testITQuadratic61 =
    "testITQuadratic61"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            (ComQuadratic 3 3)
            (2, Nothing)
            "bye so    bye?"
        ~?= "[\"\",\"\",\"bye so\",\"bye so    bye\",\"so    bye\",\"\",\"\"]"

-- String: Contains an odd palindrome
testITQuadratic62 =
    "testITQuadratic62"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            (ComQuadratic 0 9)
            (3, Just 6)
            "bye so bye"
        ~?= "bye so bye"

-- String: Contains an approximate even palindrome with punctuation
testITQuadratic63 =
    "testITQuadratic63"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            (ComQuadratic 0 2)
            (2, Just 7)
            "Fout. Weer. Hi. Hi. Niet. Goed."
        ~?= "Fout. Weer. Hi. Hi. Niet. Goed"

-- String: Contains an approximate even palindrome
testITQuadratic64 =
    "testITQuadratic64"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            (ComQuadratic 0 1)
            (0, Nothing)
            "fout weer hi hi niet goed"
        ~?= "4"

-- String: Contains odd approximate palindrome, has punctuation
testITQuadratic65 =
    "testITQuadratic65"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            (ComQuadratic 0 0)
            (2, Just 3)
            "Nope / fout / goed / niet / midden / oeps / goed / nee / ook"
        ~?= ""

-- String: Contains odd approximate palindrome
testITQuadratic66 =
    "testITQuadratic66"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            (ComQuadratic 9 9)
            (4, Nothing)
            "nope fout goed niet midden oeps goed nee ook"
        ~?= "[0,0,0,0,4,5,6,7,8,9,8,7,6,5,4,0,0,0,0]"

-- String: Contains a gapped even palindrome with punctuation
testITQuadratic67 =
    "testITQuadratic67"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            (ComQuadratic 2 0)
            (2, Just 3)
            "Doei&& hi ik b(e)n ??? een mens hi doei"
        ~?= "[0,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,0]"

-- String: Contains a gapped even palindrome
testITQuadratic68 =
    "testITQuadratic68"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            (ComQuadratic 2 1)
            (3, Just 8)
            "doei hi ik ben een mens hi doei"
        ~?= "[\"\",\"\",\"\",\"doei hi ik\",\"doei hi ik ben\",\"hi ik ben\",\"hi ik ben een\",\"ik ben een\",\"doei hi ik ben een mens hi doei\",\"ben een mens\",\"ben een mens hi\",\"een mens hi\",\"een mens hi doei\",\"mens hi doei\",\"\",\"\",\"\"]"

-- String: Contains a gapped odd palindrome with punctuation
testITQuadratic69 =
    "testITQuadratic69"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            (ComQuadratic 3 1)
            (4, Just 9)
            "dag h?i dri/e gap size. ... hi dag"
        ~?= "dag h?i dri/e gap size. ... hi dag"

-- String: Contains a gapped odd palindrome
testITQuadratic70 =
    "testITQuadratic70"
        ~: findPalindromesFormatted
            VarWord
            OutLength
            (ComQuadratic 3 0)
            (2, Just 8)
            "dag hi drie gap size hi dag"
        ~?= "7"

-- String: Contains an even palindrome with punctuation
testITQuadratic71 =
    "testITQuadratic71"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            (ComQuadratic 6 6)
            (0, Nothing)
            ",onzin, .dit pal\n pal dit/ gek"
        ~?= "[\"\",\"onzin\",\"onzin, .dit\",\"onzin, .dit pal\",\"onzin, .dit pal\n pal\",\"onzin, .dit pal\n pal dit\",\"onzin, .dit pal\n pal dit/ gek\",\"dit pal\n pal dit/ gek\",\"pal\n pal dit/ gek\",\"pal dit/ gek\",\"dit/ gek\",\"gek\",\"\"]"

-- String: Contains an even palindrome
testITQuadratic72 =
    "testITQuadratic72"
        ~: findPalindromesFormatted
            VarWord
            OutLengths
            (ComQuadratic 0 0)
            (0, Nothing)
            "onzin dit pal pal dit gek"
        ~?= "[0,1,0,1,0,1,4,1,0,1,0,1,0]"

-- String: Contains an odd palindrome with punctuation
testITQuadratic73 =
    "testITQuadratic73"
        ~: findPalindromesFormatted
            VarWord
            OutWords
            (ComQuadratic 0 1)
            (2, Just 5)
            "Onzin. pAl is. Pal gek"
        ~?= "[\"\",\"\",\"Onzin. pAl\",\"Onzin. pAl is\",\"pAl is\",\"Onzin. pAl is. Pal gek\",\"is. Pal\",\"is. Pal gek\",\"Pal gek\",\"\",\"\"]"

-- String: Contains an odd palindrome
testITQuadratic74 =
    "testITQuadratic74"
        ~: findPalindromesFormatted
            VarWord
            OutWord
            (ComQuadratic 1 2)
            (3, Nothing)
            "onzin pal is pal gek"
        ~?= "onzin pal is pal gek"
