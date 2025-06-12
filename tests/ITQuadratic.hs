{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) -}

module ITQuadratic (testListITQuadratic) where

import Test.HUnit (Test (..), (~:), (~?=))

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , OutputFilter (..)
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
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            0
            "AG\nTC"
        ~?= "[2,2,2]"

-- String: Contains a nested palindrome with punctuation
testITQuadratic2 =
    "testITQuadratic2"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            4
            "A&T-AT"
        ~?= "\"A&T-AT\""

-- String: Does not contain a palindrome
testITQuadratic3 =
    "testITQuadratic3"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 4, algMaxError = 4}
            1
            "AGTC"
        ~?= "[2,4,2]"

-- String: Contains a gapped even palindrome
testITQuadratic4 =
    "testITQuadratic4"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 2, algMaxError = 0}
            2
            "ACCT"
        ~?= "[2,4,2]"

-- String: Contains an odd-gapped dna palindrome
testITQuadratic5 =
    "testITQuadratic5"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectAll
            AlgQuadratic{algGapSize = 1, algMaxError = 2}
            3
            "“AC\nTA..,TTCT”"
        ~?= "\"AC\nT\"\n\"AC\nTA\"\n\"AC\nTA..,T\"\n\"AC\nTA..,TT\"\n\"C\nTA..,TT\"\n\"AC\nTA..,TTCT\"\n\"C\nTA..,TTCT\"\n\"A..,TTC\"\n\"A..,TTCT\"\n\"TTCT\"\n\"TCT\""

-- String: Contains an even nested palindrome
testITQuadratic6 =
    "testITQuadratic6"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            3
            "ATAT"
        ~?= "[4]"

-- String: Contains an even gapped palindrome with punctuation
testITQuadratic7 =
    "testITQuadratic7"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 4, algMaxError = 4}
            2
            "Ac\nC,T”"
        ~?= "\"Ac\nC,T\""

-- String: Contains an odd gapped palindrome
testITQuadratic8 =
    "testITQuadratic8"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 1, algMaxError = 2}
            2
            "ACTATTCT"
        ~?= "\"ACTATTCT\""

-- String: Contains an odd gapped palindrome
testITQuadratic9 =
    "testITQuadratic9"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 2, algMaxError = 0}
            1
            "AGGGT"
        ~?= "[2,2,2,2]"

-- String: Contains an odd gapped palindrome with punctuation
testITQuadratic10 =
    "testITQuadratic10"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectAll
            AlgQuadratic{algGapSize = 5, algMaxError = 5}
            0
            "A;G;G;G;T"
        ~?= "\"\"\n\"A\"\n\"A;G\"\n\"A;G;G\"\n\"A;G;G;G\"\n\"A;G;G;G;T\"\n\"G;G;G;T\"\n\"G;G;T\"\n\"G;T\"\n\"T\"\n\"\""

-- String: Contains a palindrome with punctuation
testITQuadratic11 =
    "testITQuadratic11"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            6
            "Ac.G-CgA "
        ~?= "[6]"

-- String: Contains an even palindrome
testITQuadratic12 =
    "testITQuadratic12"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            6
            "ACGCGA"
        ~?= "No palindromes found"

-- String: Contains an approximate palindrome
testITQuadratic13 =
    "testITQuadratic13"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            4
            "kabral"
        ~?= "\"abra\""

-- String: Contains no palindrome
testITQuadratic14 =
    "testITQuadratic14"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 6, algMaxError = 6}
            1
            "abcdef"
        ~?= "[1,2,3,4,5,6,5,4,3,2,1]"

-- String: Contains an even palindrome with punctuation
testITQuadratic15 =
    "testITQuadratic15"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 1, algMaxError = 2}
            3
            "a’b/ba"
        ~?= "[6]"

-- String: Contains an even palindrome
testITQuadratic16 =
    "testITQuadratic16"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 2, algMaxError = 0}
            0
            "abba"
        ~?= "\"abba\""

-- String: Contains an odd palindrome with punctuation. Contains a special character.
testITQuadratic17 =
    "testITQuadratic17"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            3
            "l.e.p’e;l"
        ~?= "[3]"

-- String: Contains an odd palindrome
testITQuadratic18 =
    "testITQuadratic18"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 5, algMaxError = 5}
            2
            "lepel"
        ~?= "\"lepel\""

-- String: Contains an approximate palindrome with punctuation
testITQuadratic19 =
    "testITQuadratic19"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            0
            "kab^ra.L"
        ~?= "[5]"

-- String: Contains no palindrome, contains punctuation
testITQuadratic20 =
    "testITQuadratic20"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            0
            "Abc'd/.ef"
        ~?= "\"A\"\n\"b\"\n\"c\"\n\"'\"\n\"d\"\n\"/\"\n\".\"\n\"e\"\n\"f\""

-- String: Contains an odd approximate palindrome, with punctuation
testITQuadratic21 =
    "testITQuadratic21"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 10, algMaxError = 10}
            2
            "zat.,s&tat"
        ~?= "[2,3,4,5,6,7,8,9,10,9,8,7,6,5,4,3,2]"

-- String: Contains an odd approximate palindrome
testITQuadratic22 =
    "testITQuadratic22"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 2, algMaxError = 0}
            2
            "zatstat"
        ~?= "[2,2,2,5,2,2,3,2]"

-- String: Contains an even gapped palindrome, with punctuation
testITQuadratic23 =
    "testITQuadratic23"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgQuadratic{algGapSize = 1, algMaxError = 2}
            5
            "blaA\\Bc..dA;b.l#i"
        ~?= "\"blaA\\\"\n\"laA\\B\"\n\"aA\\Bc\"\n\"A\\Bc.\"\n\"\\Bc..\"\n\"Bc..d\"\n\"Bc..dA\"\n\"c..dA\"\n\"..dA;\"\n\"..dA;b.\"\n\".dA;b.\"\n\"dA;b.\"\n\"A;b.l\"\n\";b.l#\"\n\"b.l#i\""

-- String: Contains an even gapped palindrome
testITQuadratic24 =
    "testITQuadratic24"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            1
            "blaABcdBAbli"
        ~?= "[1,1,1,1,1,1,1,1,1,1,1,1]"

-- String: Contains an odd gapped palindrome with punctuation
testITQuadratic25 =
    "testITQuadratic25"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 10, algMaxError = 10}
            2
            "A-B*cde)BA"
        ~?= "\"A-B*cde)BA\""

-- String: Contains an odd gapped palindrome
testITQuadratic26 =
    "testITQuadratic26"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 3, algMaxError = 2}
            4
            "ABcdeBA"
        ~?= "\"ABcdeBA\""

-- String: Contains an even palindrome, with punctuation, with special characters
testITQuadratic27 =
    "testITQuadratic27"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 2, algMaxError = 0}
            3
            "\"blaAPPab’li "
        ~?= "No palindromes found"

-- String: Contains an even palindrome
testITQuadratic28 =
    "testITQuadratic28"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgQuadratic{algGapSize = 9, algMaxError = 9}
            0
            "blaAPPAbl"
        ~?= "\"\"\n\"b\"\n\"bl\"\n\"bla\"\n\"blaA\"\n\"blaAP\"\n\"blaAPP\"\n\"blaAPPA\"\n\"blaAPPAb\"\n\"blaAPPAbl\"\n\"laAPPAbl\"\n\"aAPPAbl\"\n\"APPAbl\"\n\"PPAbl\"\n\"PAbl\"\n\"Abl\"\n\"bl\"\n\"l\"\n\"\""

-- String: Contains an odd palindrome
testITQuadratic29 =
    "testITQuadratic29"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            0
            "blaAPAbli"
        ~?= "[0,1,0,1,0,1,0,1,0,3,0,1,0,1,0,1,0,1,0]"

-- String: Contains an odd palindrome with punctuation
testITQuadratic30 =
    "testITQuadratic30"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            4
            "/blaAPa.bl.i"
        ~?= "\"aAPa\"\n\".bl.\""

-- String: Contains an even palindrome with punctuation
testITQuadratic31 =
    "testITQuadratic31"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 3, algMaxError = 2}
            2
            "/abba/"
        ~?= "\"abba\""

-- String: Conains an even gapped palindrome
testITQuadratic32 =
    "testITQuadratic32"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 2, algMaxError = 0}
            1
            "ABcdBA"
        ~?= "[6]"

-- String: Contains an even palindrome with punctuation
testITQuadratic33 =
    "testITQuadratic33"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectAll
            AlgQuadratic{algGapSize = 10, algMaxError = 5}
            0
            "bla\\AP.PA.bli"
        ~?= "\"\"\n\"\"\n\"\"\n\"bla\"\n\"\"\n\"bla\\AP\"\n\"\"\n\"bla\\AP.PA\"\n\"AP\"\n\"\"\n\"bla\\AP.PA.bli\"\n\"\"\n\"PA\"\n\"AP.PA.bli\"\n\"\"\n\"PA.bli\"\n\"\"\n\"bli\"\n\"\"\n\"\"\n\"\""

-- String: Contains an odd punctuation palindrome
testITQuadratic34 =
    "testITQuadratic34"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            3
            "bla(APA)bli"
        ~?= "[3,3,3]"

-- String: Contains an odd punctuation palindrome
testITQuadratic35 =
    "testITQuadratic35"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            2
            "lepel”"
        ~?= "[5]"

-- String: Contains an odd gapped palindrome with punctuation
testITQuadratic36 =
    "testITQuadratic36"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            2
            "AB/cde/BA"
        ~?= "No palindromes found"

-- String: Contains an approximate, even palindrome with punctuation
testITQuadratic37 =
    "testITQuadratic37"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            0
            "(ka(b)ral)"
        ~?= "[3,3]"

-- String: Contains an approximate odd palindrome
testITQuadratic38 =
    "testITQuadratic38"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            3
            "za.*ts&tat/”"
        ~?= "\"tat\""

-- String: Contains no palindrome, with punctuation
testITQuadratic39 =
    "testITQuadratic39"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 2, algMaxError = 0}
            2
            "Abc'd/.ef"
        ~?= "[2,2,2,2,2]"

-- String: Contains no palindrome
testITQuadratic40 =
    "testITQuadratic40"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 6, algMaxError = 3}
            5
            "abcdef"
        ~?= "[6]"

-- String: Contains an even palindrome, contains punctuation and special characters
testITQuadratic41 =
    "testITQuadratic41"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            1
            "a’b/ba"
        ~?= "\"a\8217b/ba\""

-- String: Contains an even palindrome
testITQuadratic42 =
    "testITQuadratic42"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            2
            "abba"
        ~?= "\"abba\""

-- String: Contains an odd palindrome, contains punctuations and special characters
testITQuadratic43 =
    "testITQuadratic43"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            2
            "l.e.p’e;l"
        ~?= "\"l.e.p’e;l\""

-- String: Contains an odd palindrome
testITQuadratic44 =
    "testITQuadratic44"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgQuadratic{algGapSize = 5, algMaxError = 2}
            3
            "lepel"
        ~?= "\"lep\"\n\"lepe\"\n\"lepel\"\n\"epel\"\n\"pel\""

-- String: Contains an approximate even palindrome
testITQuadratic45 =
    "testITQuadratic45"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 1, algMaxError = 2}
            5
            "kab^ra.L"
        ~?= "[6]"

-- String: Contains an approximate even palindrome
testITQuadratic46 =
    "testITQuadratic46"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 2, algMaxError = 0}
            0
            "kabral"
        ~?= "\"abra\""

-- String: Contains an approximate odd palindrome
testITQuadratic47 =
    "testITQuadratic47"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            1
            "zat.,s&tat"
        ~?= "[1,1,1,5,1,3,1]"

-- String: Contains an approximate odd palindrome
testITQuadratic48 =
    "testITQuadratic48"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 7, algMaxError = 7}
            2
            "zatstat"
        ~?= "\"zatstat\""

-- String: Contains an even gapped palindrome with punctuation
testITQuadratic49 =
    "testITQuadratic49"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 1}
            0
            "blaA\\Bc..dA;b.l#i"
        ~?= "[5,5,5]"

-- String: Contains an even gapped palindrome
testITQuadratic50 =
    "testITQuadratic50"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgQuadratic{algGapSize = 0, algMaxError = 0}
            2
            "blaABcdBAbli"
        ~?= "\"BAb\""

-- String: Contains an odd gapped palindrome
testITQuadratic51 =
    "testITQuadratic51"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 0, algMaxError = 8}
            6
            "A-B*cde)BA"
        ~?= "[6,7,6]"

-- String: Contains an odd gapped palindrome
testITQuadratic52 =
    "testITQuadratic52"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgQuadratic{algGapSize = 3, algMaxError = 0}
            2
            "ABcdeBA"
        ~?= "[2,3,2,3,2,7,2,3,2,3,2]"

-- String: Contains an even palindrome with punctuation
testITQuadratic53 =
    "testITQuadratic53"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgQuadratic{algGapSize = 1, algMaxError = 2}
            3
            "\"blaAPPab'li"
        ~?= "\"bla\"\n\"blaA\"\n\"blaAP\"\n\"blaAPP\"\n\"laAPP\"\n\"aAPP\"\n\"laAPPab\"\n\"blaAPPab'li\"\n\"APPab\"\n\"PPab\"\n\"PPab'l\"\n\"Pab'l\"\n\"Pab'li\"\n\"ab'li\"\n\"b'li\""

-- String: Contains an even palindrome
testITQuadratic54 =
    "testITQuadratic54"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            (AlgQuadratic 0 0)
            1
            "blaAPPAbl"
        ~?= "\"b\"\n\"l\"\n\"a\"\n\"aA\"\n\"A\"\n\"P\"\n\"APPA\"\n\"P\"\n\"A\"\n\"b\"\n\"l\""

-- String: Contains an odd palindrome
testITQuadratic55 =
    "testITQuadratic55"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            (AlgQuadratic 9 9)
            4
            "blaAPAbli"
        ~?= "[9]"

-- String: Contains an odd palindrome with punctuation
testITQuadratic56 =
    "testITQuadratic56"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            (AlgQuadratic 3 2)
            4
            "/blaAPa.bl.i.bl"
        ~?= "\"blaAPa.bl.i\""

-- String: Contains no palindromes, has punctuation
testITQuadratic57 =
    "testITQuadratic57"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            (AlgQuadratic 0 1)
            0
            "aba gdc."
        ~?= "[2]"

-- String: Contains no palindromes
testITQuadratic58 =
    "testITQuadratic58"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            (AlgQuadratic 0 0)
            0
            "aba gdc"
        ~?= "[0,1,0,1,0]"

-- String: Contains an even palindrome, has punctuation
testITQuadratic59 =
    "testITQuadratic59"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            (AlgQuadratic 0 1)
            2
            "Hello. hi hi “hello”"
        ~?= "\"Hello. hi\"\n\"Hello. hi hi\"\n\"Hello. hi hi “hello\"\n\"hi hi “hello\"\n\"hi “hello\""

-- String: Contains an even palindrome
testITQuadratic60 =
    "testITQuadratic60"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            (AlgQuadratic 1 2)
            6
            "hello hi hi hello"
        ~?= "No palindromes found"

-- String: Contains an odd palindrome, contains multiple spaces
testITQuadratic61 =
    "testITQuadratic61"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            (AlgQuadratic 3 3)
            2
            "bye so    bye?"
        ~?= "\"bye so\"\n\"bye so    bye\"\n\"so    bye\""

-- String: Contains an odd palindrome
testITQuadratic62 =
    "testITQuadratic62"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            (AlgQuadratic 0 9)
            3
            "bye so bye"
        ~?= "\"bye so bye\""

-- String: Contains an approximate even palindrome with punctuation
testITQuadratic63 =
    "testITQuadratic63"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            (AlgQuadratic 0 2)
            2
            "Fout. Weer. Hi. Hi. Niet. Goed."
        ~?= "\"Fout. Weer. Hi. Hi. Niet. Goed\""

-- String: Contains an approximate even palindrome
testITQuadratic64 =
    "testITQuadratic64"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            (AlgQuadratic 0 1)
            0
            "fout weer hi hi niet goed"
        ~?= "[4]"

-- String: Contains odd approximate palindrome, has punctuation
testITQuadratic65 =
    "testITQuadratic65"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            (AlgQuadratic 0 0)
            2
            "Nope / fout / goed / niet / midden / oeps / goed / nee / ook"
        ~?= "No palindromes found"

-- String: Contains odd approximate palindrome
testITQuadratic66 =
    "testITQuadratic66"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            (AlgQuadratic 9 9)
            4
            "nope fout goed niet midden oeps goed nee ook"
        ~?= "[4,5,6,7,8,9,8,7,6,5,4]"

-- String: Contains a gapped even palindrome with punctuation
testITQuadratic67 =
    "testITQuadratic67"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            (AlgQuadratic 2 0)
            2
            "Doei&& hi ik b(e)n ??? een mens hi doei"
        ~?= "[2,2,2,2,2,2,2]"

-- String: Contains a gapped even palindrome
testITQuadratic68 =
    "testITQuadratic68"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            (AlgQuadratic 2 1)
            3
            "doei hi ik ben een mens hi doei"
        ~?= "\"doei hi ik\"\n\"doei hi ik ben\"\n\"hi ik ben\"\n\"hi ik ben een\"\n\"ik ben een\"\n\"doei hi ik ben een mens hi doei\"\n\"ben een mens\"\n\"ben een mens hi\"\n\"een mens hi\"\n\"een mens hi doei\"\n\"mens hi doei\""

-- String: Contains a gapped odd palindrome with punctuation
testITQuadratic69 =
    "testITQuadratic69"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            (AlgQuadratic 3 1)
            4
            "dag h?i dri/e gap size. ... hi dag"
        ~?= "\"dag h?i dri/e gap size. ... hi dag\""

-- String: Contains a gapped odd palindrome
testITQuadratic70 =
    "testITQuadratic70"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            (AlgQuadratic 3 0)
            2
            "dag hi drie gap size hi dag"
        ~?= "[7]"

-- String: Contains an even palindrome with punctuation
testITQuadratic71 =
    "testITQuadratic71"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            (AlgQuadratic 6 6)
            0
            ",onzin, .dit pal\n pal dit/ gek"
        ~?= "\"\"\n\"onzin\"\n\"onzin, .dit\"\n\"onzin, .dit pal\"\n\"onzin, .dit pal\n pal\"\n\"onzin, .dit pal\n pal dit\"\n\"onzin, .dit pal\n pal dit/ gek\"\n\"dit pal\n pal dit/ gek\"\n\"pal\n pal dit/ gek\"\n\"pal dit/ gek\"\n\"dit/ gek\"\n\"gek\"\n\"\""

-- String: Contains an even palindrome
testITQuadratic72 =
    "testITQuadratic72"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            (AlgQuadratic 0 0)
            0
            "onzin dit pal pal dit gek"
        ~?= "[0,1,0,1,0,1,4,1,0,1,0,1,0]"

-- String: Contains an odd palindrome with punctuation
testITQuadratic73 =
    "testITQuadratic73"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            (AlgQuadratic 0 1)
            2
            "Onzin. pAl is. Pal gek"
        ~?= "\"Onzin. pAl\"\n\"Onzin. pAl is\"\n\"pAl is\"\n\"Onzin. pAl is. Pal gek\"\n\"is. Pal\"\n\"is. Pal gek\"\n\"Pal gek\""

-- String: Contains an odd palindrome
testITQuadratic74 =
    "testITQuadratic74"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            (AlgQuadratic 1 2)
            3
            "onzin pal is pal gek"
        ~?= "\"onzin pal is pal gek\""
