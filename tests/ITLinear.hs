{- This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.

© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring -}

module ITLinear (testListITLinear) where

import Test.HUnit (Test (..), (~:), (~?=))

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    )

testListITLinear =
    [ testITLinear1
    , testITLinear2
    , testITLinear3
    , testITLinear4
    , testITLinear5
    , testITLinear6
    , testITLinear7
    , testITLinear8
    , testITLinear9
    , testITLinear10
    , testITLinear11
    , testITLinear12
    , testITLinear13
    , testITLinear14
    , testITLinear15
    , testITLinear16
    , testITLinear17
    , testITLinear18
    , testITLinear19
    , testITLinear20
    , testITLinear21
    , testITLinear22
    , testITLinear23
    , testITLinear24
    , testITLinear25
    , testITLinear26
    , testITLinear27
    , testITLinear28
    , testITLinear29
    , testITLinear30
    , testITLinear31
    , testITLinear32
    , testITLinear33
    , testITLinear34
    , testITLinear35
    , testITLinear36
    , testITLinear37
    , testITLinear38
    , testITLinear39
    , testITLinear40
    , testITLinear41
    , testITLinear42
    , testITLinear43
    , testITLinear44
    , testITLinear45
    , testITLinear46
    , testITLinear47
    , testITLinear48
    , testITLinear49
    , testITLinear50
    , testITLinear51
    , testITLinear52
    , testITLinear53
    , testITLinear54
    , testITLinear55
    , testITLinear56
    , testITLinear57
    , testITLinear58
    , testITLinear59
    , testITLinear60
    ]

-- String: Has punctuation, is even
testITLinear1 =
    "testITLinear1"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgLinear
            0
            "A\nTAT"
        ~?= "\"A\nTAT\""

-- String: Even palindrome
testITLinear2 =
    "testITLinear2"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            AlgLinear
            10
            "abcdeedcba"
        ~?= "[10]"

-- String: Has punctuation, even nested palindromes
testITLinear3 =
    "testITLinear3"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            AlgLinear
            7
            "Word, word, word, palindrome. Word. Word, word."
        ~?= "\"Word, word, word, palindrome. Word. Word, word\""

-- String: No palindromes
testITLinear4 =
    "testITLinear4"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgLinear
            0
            "nopalindromes"
        ~?= "[0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0]"

-- String: Even, has punctuation
testITLinear5 =
    "testITLinear5"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            AlgLinear
            6
            "Has? ...A palindrome; palindrome a has."
        ~?= "[6]"

-- String: Even, has punctuation
testITLinear6 =
    "testITLinear6"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgLinear
            3
            "~ehhe~"
        ~?= "\"~ehhe~\""

-- String: Not a palindrome. Contains no punctuation
testITLinear7 =
    "testITLinear7"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgLinear
            10
            "abcdefghij"
        ~?= "No palindromes found"

-- String: Contains an even punctuation palindrome
testITLinear8 =
    "testITLinear8"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgLinear
            0
            "b abba, ...bc"
        ~?= "[4]"

-- String: Contains an even palindrome
testITLinear9 =
    "testITLinear9"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgLinear
            0
            "damittimad"
        ~?= "[0,1,0,1,0,1,0,1,0,1,10,1,0,1,0,1,0,1,0,1,0]"

-- String: Contains an even, nested palindrome
testITLinear10 =
    "testITLinear10"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgLinear
            0
            "yabaddabadoo"
        ~?= "\"abaddaba\""

-- String: Contains an even palindrome, contains punctuation
testITLinear11 =
    "testITLinear11"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectAll
            AlgLinear
            0
            "dam'itt!  \\ I'm ?ad."
        ~?= "\"\"\n\"\"\n\"\"\n\"\"\n\"\"\n\"\"\n\"\"\n\"\"\n\"\"\n\"\"\n\"dam'itt!  \\ I'm ?ad\"\n\"\"\n\"\"\n\"I\"\n\"\"\n\"m\"\n\"\"\n\"\"\n\"\"\n\"\"\n\"\""

-- String: Contains a nested odd palindrome.
testITLinear12 =
    "testITLinear12"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectLongest
            AlgLinear
            23
            "level mad dog a goddam level"
        ~?= "\"level mad dog a goddam level\""

-- String: Contains an odd palindrome
testITLinear13 =
    "testITLinear13"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectAll
            AlgLinear
            3
            "abcdcba-"
        ~?= "[7]"

-- String: Contains an even palindrome
testITLinear14 =
    "testITLinear14"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            AlgLinear
            0
            "word palindrome palindrome word"
        ~?= "\"\"\n\"word\"\n\"\"\n\"palindrome\"\n\"word palindrome palindrome word\"\n\"palindrome\"\n\"\"\n\"word\"\n\"\""

-- String: Contains an odd punctuation palindrome
testITLinear15 =
    "testITLinear15"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgLinear
            23
            "keybo{  ..  }ardmash, samdrao   byek'"
        ~?= "\"keybo{  ..  }ardmash, samdrao   byek\""

-- String: Contains no palindrome, contains punctuation
testITLinear16 =
    "testITLinear16"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgLinear
            1
            "a#b, ,c^D efg[h"
        ~?= "\"a\"\n\"b\"\n\"c\"\n\"D\"\n\"e\"\n\"f\"\n\"g\"\n\"h\""

-- String: Contains no palindrome
testITLinear17 =
    "testITLinear17"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgLinear
            3
            "nopalindromes"
        ~?= "No palindromes found"

-- String: Contains an odd palindrome with punctuation
testITLinear18 =
    "testITLinear18"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            AlgLinear
            2
            "Odd palindrome, odd- ly enough :)"
        ~?= "\"Odd palindrome, odd\""

-- String: Contains an even punctuation palindrome
testITLinear19 =
    "testITLinear19"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectLongest
            AlgLinear
            2
            ".ehhe~"
        ~?= "[4]"

-- String: Contains an even nested palindrome
testITLinear20 =
    "testITLinear20"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgLinear
            4
            "ATATGCGCATAT"
        ~?= "[12]"

-- String: Contains an even nested palindrome
testITLinear21 =
    "testITLinear21"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            AlgLinear
            10
            "That that, that that that -that that that refers to-"
        ~?= "No palindromes found"

-- String: Contains an odd palindrome
testITLinear22 =
    "testITLinear22"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            AlgLinear
            5
            "A simple palindrome. Simple, a (ye)?"
        ~?= "[5]"

-- String: Contains a nested even palindrome
testITLinear23 =
    "testITLinear23"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgLinear
            2
            "ATTCGGCGCAAT"
        ~?= "[2,2,2,4,2,2]"

-- String: Contains an even palindrom with punctuation
testITLinear24 =
    "testITLinear24"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgLinear
            6
            "ab:c|+cba..."
        ~?= "\"ab:c|+cba\""

-- String: Contains an odd nested palindrome
testITLinear25 =
    "testITLinear25"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgLinear
            0
            "ennestedetsenne"
        ~?= "\"\"\n\"e\"\n\"\"\n\"n\"\n\"enne\"\n\"n\"\n\"\"\n\"e\"\n\"\"\n\"s\"\n\"\"\n\"t\"\n\"\"\n\"e\"\n\"\"\n\"ennestedetsenne\"\n\"\"\n\"e\"\n\"\"\n\"t\"\n\"\"\n\"s\"\n\"\"\n\"e\"\n\"\"\n\"n\"\n\"enne\"\n\"n\"\n\"\"\n\"e\"\n\"\""

-- String: Contains an odd palindrome with punctuation
testITLinear26 =
    "testITLinear26"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgLinear
            7
            "*abc-dc ba#"
        ~?= "[7]"

-- String: Contains a nested odd palindrome
testITLinear27 =
    "testITLinear27"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectLongest
            AlgLinear
            0
            "nested odd word odd palindrome odd word odd nested"
        ~?= "[9]"

-- String: Contains an even palindrome, contains punctuation
testITLinear28 =
    "testITLinear28"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgLinear
            0
            "TATG\nCGATC  GCA.TAGC"
        ~?= "[14]"

-- String: Contains an even nested palindrome with punctuation
testITLinear29 =
    "testITLinear29"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectLongest
            AlgLinear
            0
            "d*eif:ied a-+bba deifie;d!"
        ~?= "\"d*eif:ied a-+bba deifie;d\""

-- String: Contains an odd nested palindrome
testITLinear30 =
    "testITLinear30"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgLinear
            0
            "abcbababcba"
        ~?= "[0,1,0,1,0,5,0,1,0,3,0,11,0,3,0,1,0,5,0,1,0,1,0]"

-- String: Contains an even nested palindrome with punctuation
testITLinear31 =
    "testITLinear31"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgLinear
            4
            "AT\nATAT-A\tT"
        ~?= "[8]"

-- String: Contains an even palindrome
testITLinear32 =
    "testITLinear32"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectLongest
            AlgLinear
            18
            "evil is a deed as I live"
        ~?= "\"evil is a deed as I live\""

-- String: Contains an even nested palindrome with punctuation
testITLinear33 =
    "testITLinear33"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgLinear
            0
            "abbaccabba"
        ~?= "[0,1,0,1,4,1,0,1,0,1,10,1,0,1,0,1,4,1,0,1,0]"

-- String: Contains an even palindrome with punctuation
testITLinear34 =
    "testITLinear34"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectAll
            AlgLinear
            14
            "TATG\nCGATC  GCA.TA"
        ~?= "\"TATG\nCGATC  GCA.TA\""

-- String: Contains no palindrome, contains punctuation
testITLinear35 =
    "testITLinear35"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            AlgLinear
            3
            "Thi,s is not a p-alindro'me."
        ~?= "No palindromes found"

-- String: Contains an odd palindrome with punctuation
testITLinear36 =
    "testITLinear36"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            AlgLinear
            3
            "A simple palindrome. Simple, a (ye)?"
        ~?= "\"A simple palindrome. Simple, a\""

-- String: Contains an even nested palindrome
testITLinear37 =
    "testITLinear37"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectAll
            AlgLinear
            1
            "d*eif:ied a-+bba deifie;d!"
        ~?= "\"d\"\n\"d*eif:ied\"\n\"a\"\n\"d*eif:ied a-+bba deifie;d\"\n\"deifie;d\"\n\"d\""

-- String: Contains no palindrome, contains punctuation
testITLinear38 =
    "testITLinear38"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectAll
            AlgLinear
            21
            "Thi,s is not a p-alindro'me."
        ~?= "No palindromes found"

-- String: Contains an even palindrome
testITLinear39 =
    "testITLinear39"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgLinear
            0
            "ATGCGCATTA"
        ~?= "\"ATGCGCAT\""

-- String: Contains no palindrome, contains punctuation
testITLinear40 =
    "testITLinear40"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectAll
            AlgLinear
            2
            "AN\nTC\tAGTC"
        ~?= "No palindromes found"

-- String: Contains no palindrome
testITLinear41 =
    "testITLinear41"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgLinear
            1
            "NNNNNNNNNN"
        ~?= "No palindromes found"

-- String: Contains an odd punctuation palindrome
testITLinear42 =
    "testITLinear42"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectAll
            AlgLinear
            0
            "\nodddo!"
        ~?= "[0,1,0,1,0,1,2,5,2,1,0,1,0,1,0]"

-- String: Contains an odd palindrome
testITLinear43 =
    "testITLinear43"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgLinear
            1
            "releveler"
        ~?= "\"r\"\n\"e\"\n\"ele\"\n\"e\"\n\"releveler\"\n\"e\"\n\"ele\"\n\"e\"\n\"r\""

-- String: Contains an odd nested palindrome
testITLinear44 =
    "testITLinear44"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectLongest
            AlgLinear
            0
            "hi madam im adam bye"
        ~?= "\"madam im adam\""

-- String: Contains an odd nested palindrome
testITLinear45 =
    "testITLinear45"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectAll
            AlgLinear
            3
            "Oi! You there! You! Stop!"
        ~?= "\"You there! You\""

-- String: Contains an odd nested palindrome
testITLinear46 =
    "testITLinear46"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatText
            SelectLongest
            AlgLinear
            0
            "You! Rev Ile deliver, now!"
        ~?= "\"Rev Ile deliver\""

-- String: Contains an odd palindrome with punctuation
testITLinear47 =
    "testITLinear47"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgLinear
            0
            "Revile.... Deliver!"
        ~?= "[0,1,0,1,0,1,0,1,0,1,0,1,0,13,0,1,0,1,0,1,0,1,0,1,0,1,0]"

-- String: Contains an even nested palindrome with punctuation
testITLinear48 =
    "testITLinear48"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectAll
            AlgLinear
            0
            "It's high noon..."
        ~?= "\"\"\n\"I\"\n\"\"\n\"t\"\n\"\"\n\"'\"\n\"\"\n\"s\"\n\"\"\n\" \"\n\"\"\n\"h\"\n\"\"\n\"i\"\n\"\"\n\"g\"\n\"\"\n\"h\"\n\"\"\n\" \"\n\"\"\n\"n\"\n\"\"\n\"o\"\n\"noon\"\n\"o\"\n\"\"\n\"n\"\n\"\"\n\".\"\n\"..\"\n\"...\"\n\"..\"\n\".\"\n\"\""

-- String: Contains an odd palindrome with punctuation
testITLinear49 =
    "testITLinear49"
        ~: findPalindromesFormatted
            VarPlain
            FormatText
            SelectLongest
            AlgLinear
            3
            "!dammit_timmad!"
        ~?= "\"!dammit_timmad!\""

-- String: Contains no punctuation palindrome
testITLinear50 =
    "testITLinear50"
        ~: findPalindromesFormatted
            VarPunctuation
            FormatLength
            SelectLongest
            AlgLinear
            0
            "begone enoge"
        ~?= "[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"

-- String: Contains an odd and an even palindrome
testITLinear51 =
    "testITLinear51"
        ~: findPalindromesFormatted
            VarText
            FormatLength
            SelectAll
            AlgLinear
            2
            "madamoiselle"
        ~?= "[5,4]"

-- String: Contains an even palindrome with punctuation
testITLinear52 =
    "testITLinear52"
        ~: findPalindromesFormatted
            VarWord
            FormatLength
            SelectAll
            AlgLinear
            4
            "Hello? my.... my .. hello!"
        ~?= "[4]"

-- String: Contains an even punctuation palindrome
testITLinear53 =
    "testITLinear53"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgLinear
            2
            "\nAT  CG /AT/"
        ~?= "[6]"

-- String: Contains an even palindrome
testITLinear54 =
    "testITLinear54"
        ~: findPalindromesFormatted
            VarDNA
            FormatText
            SelectLongest
            AlgLinear
            4
            "CGCG"
        ~?= "\"CGCG\""

-- String: Contains an odd nested palindrome
testITLinear55 =
    "testITLinear55"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgLinear
            0
            "hi madam i madam bye"
        ~?= "[15]"

-- String: Contains an even nested palindrome
testITLinear56 =
    "testITLinear56"
        ~: findPalindromesFormatted
            VarDNA
            FormatLength
            SelectLongest
            AlgLinear
            6
            "NNTATATATANN"
        ~?= "[8]"

-- String: Contains an even palindrome with punctuation
testITLinear57 =
    "testITLinear57"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgLinear
            5
            "!ma;dd;am!"
        ~?= "[10]"

-- String: Contains no palindrome
testITLinear58 =
    "testITLinear58"
        ~: findPalindromesFormatted
            VarWord
            FormatText
            SelectLongest
            AlgLinear
            2
            "word1 word2 word3 word4"
        ~?= "No palindromes found"

-- String: Contains an odd palindrome
testITLinear59 =
    "testITLinear59"
        ~: findPalindromesFormatted
            VarText
            FormatText
            SelectAll
            AlgLinear
            9
            "Releveler"
        ~?= "\"Releveler\""

-- String: Contains an odd punctuation palindrome
testITLinear60 =
    "testITLinear60"
        ~: findPalindromesFormatted
            VarPlain
            FormatLength
            SelectLongest
            AlgLinear
            5
            "\nOdd do!"
        ~?= "No palindromes found"
