{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring -}

module ITApproximate (testListITApproximate) where

import Test.HUnit (Test (..), (~:), (~?=))

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    )

testListITApproximate =
    [ testITApproximate1
    , testITApproximate2
    , testITApproximate3
    , testITApproximate4
    , testITApproximate5
    , testITApproximate6
    , testITApproximate7
    , testITApproximate8
    , testITApproximate9
    , testITApproximate10
    , testITApproximate11
    , testITApproximate12
    , testITApproximate13
    , testITApproximate14
    , testITApproximate15
    , testITApproximate16
    , testITApproximate17
    , testITApproximate18
    , testITApproximate19
    , testITApproximate20
    , testITApproximate21
    , testITApproximate22
    , testITApproximate23
    , testITApproximate24
    , testITApproximate25
    , testITApproximate26
    , testITApproximate27
    , testITApproximate28
    , testITApproximate29
    , testITApproximate30
    , testITApproximate31
    , testITApproximate32
    , testITApproximate33
    , testITApproximate34
    , testITApproximate35
    , testITApproximate36
    , testITApproximate37
    , testITApproximate38
    , testITApproximate39
    , testITApproximate40
    , testITApproximate41
    , testITApproximate42
    , testITApproximate43
    , testITApproximate44
    , testITApproximate45
    , testITApproximate46
    , testITApproximate47
    , testITApproximate48
    , testITApproximate49
    , testITApproximate50
    , testITApproximate51
    , testITApproximate52
    , testITApproximate53
    , testITApproximate54
    , testITApproximate55
    , testITApproximate56
    , testITApproximate57
    , testITApproximate58
    , testITApproximate59
    , testITApproximate60
    , testITApproximate61
    , testITApproximate62
    , testITApproximate63
    , testITApproximate64
    , testITApproximate65
    , testITApproximate66
    , testITApproximate67
    , testITApproximate68
    , testITApproximate69
    , testITApproximate70
    , testITApproximate71
    , testITApproximate72
    , testITApproximate73
    , testITApproximate74
    ]

testITApproximate1 =
    "testITApproximate1"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            0
            "AG\nTC"
        ~?= "[3,3]"

-- String: Contains a nested palindrome with punctuation
testITApproximate2 =
    "testITApproximate2"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            4
            "A&T-AT"
        ~?= "\"A&T-AT\""

-- String: Does not contain a palindrome
testITApproximate3 =
    "testITApproximate3"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 4}
            1
            "AGTC"
        ~?= "[4]"

-- String: Contains a gapped even palindrome
testITApproximate4 =
    "testITApproximate4"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 2, algMaxErrors = 0}
            2
            "ACCT"
        ~?= "[2,2,4]"

-- String: Contains an odd-gapped dna palindrome
testITApproximate5 =
    "testITApproximate5"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 1, algMaxErrors = 2}
            3
            "“AC\nTA..,TTCT”"
        ~?= "\"AC\nTA..,TTCT\""

-- String: Contains an even nested palindrome
testITApproximate6 =
    "testITApproximate6"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            3
            "ATAT"
        ~?= "[4]"

-- String: Contains an even gapped palindrome with punctuation
testITApproximate7 =
    "testITApproximate7"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 4, algMaxErrors = 4}
            2
            "Ac\nC,T”"
        ~?= "\"Ac\nC,T\""

-- String: Contains an odd gapped palindrome
testITApproximate8 =
    "testITApproximate8"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 1, algMaxErrors = 2}
            2
            "ACTATTCT"
        ~?= "\"ACTATTCT\""

-- String: Contains an odd gapped palindrome
testITApproximate9 =
    "testITApproximate9"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 2, algMaxErrors = 0}
            1
            "AGGGT"
        ~?= "[2,2,2,2]"

-- String: Contains an odd gapped palindrome with punctuation
testITApproximate10 =
    "testITApproximate10"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 5, algMaxErrors = 5}
            0
            "A;G;G;G;T"
        ~?= "\"A;G;G;G;T\""

-- String: Contains a palindrome with punctuation
testITApproximate11 =
    "testITApproximate11"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            6
            "Ac.G-CgA "
        ~?= "[6]"

-- String: Contains an even palindrome
testITApproximate12 =
    "testITApproximate12"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            6
            "ACGCGA"
        ~?= "No palindromes found"

-- String: Contains an approximate palindrome
testITApproximate13 =
    "testITApproximate13"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            4
            "kabral"
        ~?= "\"abra\""

-- String: Contains no palindrome
testITApproximate14 =
    "testITApproximate14"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 6}
            1
            "abcdef"
        ~?= "[6]"

-- String: Contains an even palindrome with punctuation
testITApproximate15 =
    "testITApproximate15"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 2}
            3
            "a’b/ba"
        ~?= "[6]"

-- String: Contains an even palindrome
testITApproximate16 =
    "testITApproximate16"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            0
            "abba"
        ~?= "\"abba\""

-- String: Contains an odd palindrome with punctuation. Contains a special character.
testITApproximate17 =
    "testITApproximate17"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            3
            "l.e.p’e;l"
        ~?= "[3]"

-- String: Contains an odd palindrome
testITApproximate18 =
    "testITApproximate18"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 5, algMaxErrors = 5}
            2
            "lepel"
        ~?= "\"lepel\""

-- String: Contains an approximate palindrome with punctuation
testITApproximate19 =
    "testITApproximate19"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            0
            "kab^ra.L"
        ~?= "[5]"

-- String: Contains no palindrome, contains punctuation
testITApproximate20 =
    "testITApproximate20"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            0
            "Abc'd/.ef"
        ~?= "\"f\"\n\"e\"\n\".\"\n\"/\"\n\"d\"\n\"'\"\n\"c\"\n\"b\"\n\"A\""

-- String: Contains an odd approximate palindrome, with punctuation
testITApproximate21 =
    "testITApproximate21"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 10}
            2
            "zat.,s&tat"
        ~?= "[10]"

-- String: Contains an odd approximate palindrome
testITApproximate22 =
    "testITApproximate22"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            2
            "zatstat"
        ~?= "[3,5]"

-- String: Contains an even gapped palindrome, with punctuation
testITApproximate23 =
    "testITApproximate23"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 1, algMaxErrors = 2}
            5
            "blaA\\Bc..dA;b.l#i"
        ~?= "\"b.l#i\"\n\";b.l#\"\n\"A;b.l\"\n\"..dA;\"\n\"..dA;b.\"\n\"Bc..dA\"\n\"\\Bc..\"\n\"A\\Bc.\"\n\"aA\\Bc\"\n\"laA\\B\"\n\"blaA\\\""

-- String: Contains an even gapped palindrome
testITApproximate24 =
    "testITApproximate24"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 2, algMaxErrors = 0}
            1
            "blaABcdBAbli"
        ~?= "[2,2,2,2,2,2,2,6,2,2,2]"

-- String: Contains an odd gapped palindrome with punctuation
testITApproximate25 =
    "testITApproximate25"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 10, algMaxErrors = 10}
            2
            "A-B*cde)BA"
        ~?= "\"A-B*cde)BA\""

-- String: Contains an odd gapped palindrome
testITApproximate26 =
    "testITApproximate26"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 3, algMaxErrors = 2}
            4
            "ABcdeBA"
        ~?= "\"ABcdeBA\""

-- String: Contains an even palindrome, with punctuation, with special characters
testITApproximate27 :: Test
testITApproximate27 =
    "testITApproximate27"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            3
            "\"blaAPPab’li "
        ~?= "No palindromes found"

-- String: Contains an even palindrome
testITApproximate28 =
    "testITApproximate28"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 9}
            0
            "blaAPPAbl"
        ~?= "\"blaAPPAbl\""

-- String: Contains an odd palindrome
testITApproximate29 =
    "testITApproximate29"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            0
            "blaAPAbli"
        ~?= "[1,1,1,1,1,3,1,1,1]"

-- String: Contains an odd palindrome with punctuation
testITApproximate30 =
    "testITApproximate30"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            4
            "/blaAPa.bl.i"
        ~?= "\".bl.\"\n\"aAPa\""

-- String: Contains an even palindrome with punctuation
testITApproximate31 =
    "testITApproximate31"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 2}
            2
            "/abba/"
        ~?= "\"abba\""

-- String: Conains an even gapped palindrome
testITApproximate32 =
    "testITApproximate32"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 2, algMaxErrors = 0}
            1
            ".,.ABcdBA,a"
        ~?= "[6]"

-- String: Contains an even palindrome with punctuation
testITApproximate33 =
    "testITApproximate33"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 5}
            0
            "bla\\AP.PA.bli"
        ~?= "\"bla\\AP.PA.bli\""

-- String: Contains an odd punctuation palindrome
testITApproximate34 =
    "testITApproximate34"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            4
            "blaAPA)bli"
        ~?= "No palindromes found"

-- String: Contains an odd punctuation palindrome
testITApproximate35 =
    "testITApproximate35"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            2
            "lepel”"
        ~?= "[5]"

-- String: Contains an odd gapped palindrome with punctuation
testITApproximate36 =
    "testITApproximate36"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            2
            "AB/cde/BA"
        ~?= "No palindromes found"

-- String: Contains an approximate, even palindrome with punctuation
testITApproximate37 =
    "testITApproximate37"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            0
            "(ka(b)ral)"
        ~?= "[3,3]"

-- String: Contains an approximate odd palindrome
testITApproximate38 =
    "testITApproximate38"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            3
            "za.*ts&tat/”"
        ~?= "\"tat\""

-- String: Contains no palindrome, with punctuation
testITApproximate39 =
    "testITApproximate39"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            2
            "Abc'd/.ef"
        ~?= "No palindromes found"

-- String: Contains no palindrome
testITApproximate40 =
    "testITApproximate40"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 3}
            5
            "abcdef"
        ~?= "[6]"

-- String: Contains an even palindrome, contains punctuation and special characters
testITApproximate41 =
    "testITApproximate41"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            1
            "a’b/ba"
        ~?= "\"a’b/ba\""

-- String: Contains an even palindrome
testITApproximate42 =
    "testITApproximate42"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            2
            "abba"
        ~?= "\"abba\""

-- String: Contains an odd palindrome, contains punctuations and special characters
testITApproximate43 =
    "testITApproximate43"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            2
            "l.e.p’e;l"
        ~?= "\"l.e.p’e;l\""

-- String: Contains an odd palindrome
testITApproximate44 =
    "testITApproximate44"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 2}
            3
            "lepel"
        ~?= "\"lepel\""

-- String: Contains an approximate even palindrome
testITApproximate45 =
    "testITApproximate45"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 2}
            6
            "kab^ra.L"
        ~?= "[6]"

-- String: Contains an approximate even palindrome
testITApproximate46 =
    "testITApproximate46"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 2, algMaxErrors = 0}
            0
            "kabral"
        ~?= "\"abra\""

-- String: Contains an approximate odd palindrome
testITApproximate47 =
    "testITApproximate47"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            1
            "zat.,s&tat"
        ~?= "[1,1,3,1,1,5,1]"

-- String: Contains an approximate odd palindrome
testITApproximate48 =
    "testITApproximate48"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 7}
            2
            "zatstat"
        ~?= "\"zatstat\""

-- String: Contains an even gapped palindrome with punctuation
testITApproximate49 =
    "testITApproximate49"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            0
            "blaA\\Bc..dA;b.l#i"
        ~?= "[5,5,5]"

-- String: Contains an even gapped palindrome
testITApproximate50 =
    "testITApproximate50"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            2
            "blaABcdBAbli"
        ~?= "\"BAb\""

-- String: Contains an odd gapped palindrome
testITApproximate51 =
    "testITApproximate51"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 8}
            6
            "A-B*cde)BA"
        ~?= "[7]"

-- String: Contains an odd gapped palindrome
testITApproximate52 =
    "testITApproximate52"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 3, algMaxErrors = 0}
            2
            "ABcdeBA"
        ~?= "[3,3,3,3,7]"

-- String: Contains an even palindrome with punctuation
testITApproximate53 =
    "testITApproximate53"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 2}
            3
            "\"blaAPPab'li"
        ~?= "\"Pab'li\"\n\"blaAPP\"\n\"blaAPPab'li\""

-- String: Contains an even palindrome
testITApproximate54 =
    "testITApproximate54"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            1
            "blaAPPAbl"
        ~?= "\"l\"\n\"b\"\n\"A\"\n\"APPA\"\n\"aA\"\n\"l\"\n\"b\""

-- String: Contains an odd palindrome
testITApproximate55 =
    "testITApproximate55"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 9}
            4
            "blaAPAbli"
        ~?= "[9]"

-- String: Contains an odd palindrome with punctuation
testITApproximate56 =
    "testITApproximate56"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 3, algMaxErrors = 2}
            4
            "/blaAPa.bl.i.bl"
        ~?= "\"Pa.bl.i.bl\"\n\"APa.bl.i.b\"\n\"blaAPa.bl.i.b\""

-- String: Contains no palindromes, has punctuation
testITApproximate57 =
    "testITApproximate57"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            0
            "aba gdc."
        ~?= "[2]"

-- String: Contains no palindromes
testITApproximate58 =
    "testITApproximate58"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            0
            "aba gdc"
        ~?= "[1,1]"

-- String: Contains an even palindrome, has punctuation
testITApproximate59 =
    "testITApproximate59"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            2
            "Hello. hi hi “hello”"
        ~?= "\"Hello. hi hi “hello\""

-- String: Contains an even palindrome
testITApproximate60 =
    "testITApproximate60"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 2}
            6
            "hello hi hi hello"
        ~?= "No palindromes found"

-- String: Contains an odd palindrome, contains multiple spaces
testITApproximate61 =
    "testITApproximate61"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 3}
            2
            "bye so    bye?"
        ~?= "\"bye so    bye\""

-- String: Contains an odd palindrome
testITApproximate62 =
    "testITApproximate62"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 9}
            3
            "bye so bye"
        ~?= "\"bye so bye\""

-- String: Contains an approximate even palindrome with punctuation
testITApproximate63 =
    "testITApproximate63"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 2}
            2
            "Fout. Weer. Hi. Hi. Niet. Goed."
        ~?= "\"Fout. Weer. Hi. Hi. Niet. Goed\""

-- String: Contains an approximate even palindrome
testITApproximate64 =
    "testITApproximate64"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            0
            "fout weer hi hi niet goed"
        ~?= "[4]"

-- String: Contains odd approximate palindrome, has punctuation
testITApproximate65 =
    "testITApproximate65"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            2
            "Nope / fout / goed / niet / midden / oeps / goed / nee / ook"
        ~?= "No palindromes found"

-- String: Contains odd approximate palindrome
testITApproximate66 =
    "testITApproximate66"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 9}
            4
            "nope fout goed niet midden oeps goed nee ook"
        ~?= "[9]"

-- String: Contains a gapped even palindrome with punctuation
testITApproximate67 =
    "testITApproximate67"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 2, algMaxErrors = 0}
            2
            "Doei&& hi ik b(e)n ??? een mens hi doei"
        ~?= "[2,2,2,2,2,2,2]"

-- String: Contains a gapped even palindrome
testITApproximate68 =
    "testITApproximate68"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 2, algMaxErrors = 1}
            3
            "doei hi ik ben een mens hi doei"
        ~?= "\"een mens hi doei\"\n\"ben een mens hi\"\n\"hi ik ben een\"\n\"doei hi ik ben\"\n\"doei hi ik ben een mens hi doei\""

-- String: Contains a gapped odd palindrome with punctuation
testITApproximate69 =
    "testITApproximate69"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 3, algMaxErrors = 1}
            4
            "dag h?i dri/e gap size. ... hi dag"
        ~?= "\"dag h?i dri/e gap size. ... hi dag\""

-- String: Contains a gapped odd palindrome
testITApproximate70 =
    "testITApproximate70"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            AlgApproximate{algGapSize = 3, algMaxErrors = 0}
            2
            "dag hi drie gap size hi dag"
        ~?= "[7]"

-- String: Contains an even palindrome with punctuation
testITApproximate71 =
    "testITApproximate71"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 6}
            0
            ",onzin, .dit pal\n pal dit/ gek"
        ~?= "\"onzin, .dit pal\n pal dit/ gek\""

-- String: Contains an even palindrome
testITApproximate72 =
    "testITApproximate72"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 0}
            0
            "onzin dit pal pal dit gek"
        ~?= "[1,1,1,4,1]"

-- String: Contains an odd palindrome with punctuation
testITApproximate73 =
    "testITApproximate73"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            AlgApproximate{algGapSize = 0, algMaxErrors = 1}
            2
            "Onzin. pAl is. Pal gek"
        ~?= "\"Onzin. pAl is. Pal gek\""

-- String: Contains an odd palindrome
testITApproximate74 =
    "testITApproximate74"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            AlgApproximate{algGapSize = 0, algMaxErrors = 2}
            3
            "onzin pal is pal gek"
        ~?= "\"onzin pal is pal gek\""
