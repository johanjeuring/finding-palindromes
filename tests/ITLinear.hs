module ITLinear where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (..)
    , OutputFormat (..)
    , Variant (..)
    , createReadableCombinator
    )
import Test.HUnit (Test (..), (~:), (~?=))

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
        ~: createReadableCombinator
            VarDNA
            OutWord
            ComLinear
            (0, Just 0)
            "A\nTAT"
        ~?= ""

-- String: Even palindrome
testITLinear2 =
    "testITLinear2"
        ~: createReadableCombinator
            VarText
            OutLength
            ComLinear
            (10, Nothing)
            "abcdeedcba"
        ~?= "10"

-- String: Has punctuation, even nested palindromes
testITLinear3 =
    "testITLinear3"
        ~: createReadableCombinator
            VarWord
            OutWords
            ComLinear
            (7, Just 7)
            "Word, word, word, palindrome. Word. Word, word."
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"Word, word, word, palindrome. Word. Word, word\",\"\",\"\",\"\",\"\",\"\",\"\",\"\"]"

-- String: No palindromes
testITLinear4 =
    "testITLinear4"
        ~: createReadableCombinator
            VarText
            OutLengths
            ComLinear
            (0, Just 3)
            "nopalindromes"
        ~?= "[0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0]"

-- String: Even, has punctuation
testITLinear5 =
    "testITLinear5"
        ~: createReadableCombinator
            VarWord
            OutLengths
            ComLinear
            (6, Just 6)
            "Has? ...A palindrome; palindrome a has."
        ~?= "[0,0,0,0,0,0,6,0,0,0,0,0,0]"

-- String: Even, has punctuation
testITLinear6 =
    "testITLinear6"
        ~: createReadableCombinator
            VarPlain
            OutWords
            ComLinear
            (3, Just 4)
            "~ehhe~"
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Not a palindrome. Contains no punctuation
testITLinear7 =
    "testITLinear7"
        ~: createReadableCombinator
            VarPlain
            OutLength
            ComLinear
            (10, Nothing)
            "abcdefghij"
        ~?= "0"

-- String: Contains an even punctuation palindrome
testITLinear8 =
    "testITLinear8"
        ~: createReadableCombinator
            VarPunctuation
            OutLength
            ComLinear
            (0, Nothing)
            "b abba, ...bc"
        ~?= "4"

-- String: Contains an even palindrome
testITLinear9 =
    "testITLinear9"
        ~: createReadableCombinator
            VarPlain
            OutLengths
            ComLinear
            (0, Just 3)
            "damittimad"
        ~?= "[0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0]"

-- String: Contains an even, nested palindrome
testITLinear10 =
    "testITLinear10"
        ~: createReadableCombinator
            VarPlain
            OutWord
            ComLinear
            (0, Just 7)
            "yabaddabadoo"
        ~?= "dabad"

-- String: Contains an even palindrome, contains punctuation
testITLinear11 =
    "testITLinear11"
        ~: createReadableCombinator
            VarPunctuation
            OutWords
            ComLinear
            (0, Nothing)
            "dam'itt!  \\ I'm ?ad."
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"dam'itt!  \\ I'm ?ad\",\"\",\"\",\"I\",\"\",\"m\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Contains a nested odd palindrome.
testITLinear12 =
    "testITLinear12"
        ~: createReadableCombinator
            VarPunctuation
            OutWord
            ComLinear
            (23, Just 25)
            "level mad dog a goddam level"
        ~?= "level mad dog a goddam level"

-- String: Contains an odd palindrome
testITLinear13 =
    "testITLinear13"
        ~: createReadableCombinator
            VarPunctuation
            OutLengths
            ComLinear
            (3, Nothing)
            "abcdcba-"
        ~?= "[0,0,0,0,0,0,0,7,0,0,0,0,0,0,0]"

-- String: Contains an even palindrome
testITLinear14 =
    "testITLinear14"
        ~: createReadableCombinator
            VarWord
            OutWords
            ComLinear
            (0, Just 2)
            "word palindrome palindrome word"
        ~?= "[\"\",\"word\",\"\",\"palindrome\",\"\",\"palindrome\",\"\",\"word\",\"\"]"

-- String: Contains an odd punctuation palindrome
testITLinear15 =
    "testITLinear15"
        ~: createReadableCombinator
            VarText
            OutWord
            ComLinear
            (23, Just 28)
            "keybo{  ..  }ardmash, samdrao   byek'"
        ~?= "keybo{  ..  }ardmash, samdrao   byek"

-- String: Contains no palindrome, contains punctuation
testITLinear16 =
    "testITLinear16"
        ~: createReadableCombinator
            VarText
            OutWords
            ComLinear
            (0, Just 8)
            "a#b, ,c^D efg[h"
        ~?= "[\"\",\"a\",\"\",\"b\",\"\",\"c\",\"\",\"D\",\"\",\"e\",\"\",\"f\",\"\",\"g\",\"\",\"h\",\"\"]"

-- String: Contains no palindrome
testITLinear17 =
    "testITLinear17"
        ~: createReadableCombinator
            VarPlain
            OutWords
            ComLinear
            (3, Nothing)
            "nopalindromes"
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Contains an odd palindrome with punctuation
testITLinear18 =
    "testITLinear18"
        ~: createReadableCombinator
            VarWord
            OutWord
            ComLinear
            (2, Nothing)
            "Odd palindrome, odd- ly enough :)"
        ~?= "Odd palindrome, odd"

-- String: Contains an even punctuation palindrome
testITLinear19 =
    "testITLinear19"
        ~: createReadableCombinator
            VarText
            OutLength
            ComLinear
            (2, Just 5)
            ".ehhe~"
        ~?= "4"

-- String: Contains an even nested palindrome
testITLinear20 =
    "testITLinear20"
        ~: createReadableCombinator
            VarDNA
            OutLength
            ComLinear
            (4, Just 9)
            "ATATGCGCATAT"
        ~?= "4"

-- String: Contains an even nested palindrome
testITLinear21 =
    "testITLinear21"
        ~: createReadableCombinator
            VarWord
            OutLength
            ComLinear
            (10, Just 0)
            "That that, that that that -that that that refers to-"
        ~?= "0"

-- String: Contains an odd palindrome
testITLinear22 =
    "testITLinear22"
        ~: createReadableCombinator
            VarWord
            OutLength
            ComLinear
            (5, Just 8)
            "A simple palindrome. Simple, a (ye)?"
        ~?= "5"

-- String: Contains a nested even palindrome
testITLinear23 =
    "testITLinear23"
        ~: createReadableCombinator
            VarDNA
            OutLengths
            ComLinear
            (2, Nothing)
            "ATTCGGCGCAAT"
        ~?= "[0,2,0,0,2,0,2,4,2,0,0,2,0]"

-- String: Contains an even palindrom with punctuation
testITLinear24 =
    "testITLinear24"
        ~: createReadableCombinator
            VarText
            OutWord
            ComLinear
            (6, Just 10)
            "ab:c|+cba..."
        ~?= "ab:c|+cba"

-- String: Contains an odd nested palindrome
testITLinear25 =
    "testITLinear25"
        ~: createReadableCombinator
            VarPlain
            OutWords
            ComLinear
            (0, Just 0)
            "ennestedetsenne"
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Contains an odd palindrome with punctuation
testITLinear26 =
    "testITLinear26"
        ~: createReadableCombinator
            VarPunctuation
            OutLength
            ComLinear
            (7, Just 8)
            "*abc-dc ba#"
        ~?= "7"

-- String: Contains a nested odd palindrome
testITLinear27 =
    "testITLinear27"
        ~: createReadableCombinator
            VarWord
            OutLength
            ComLinear
            (0, Nothing)
            "nested odd word odd palindrome odd word odd nested"
        ~?= "9"

-- String: Contains an even palindrome, contains punctuation
testITLinear28 =
    "testITLinear28"
        ~: createReadableCombinator
            VarDNA
            OutLength
            ComLinear
            (0, Nothing)
            "TATG\nCGATC  GCA.TAGC"
        ~?= "14"

-- String: Contains an even nested palindrome with punctuation
testITLinear29 =
    "testITLinear29"
        ~: createReadableCombinator
            VarPunctuation
            OutWord
            ComLinear
            (0, Just 8)
            "d*eif:ied a-+bba deifie;d!"
        ~?= "d*eif:ied"

-- String: Contains an odd nested palindrome
testITLinear30 =
    "testITLinear30"
        ~: createReadableCombinator
            VarText
            OutLengths
            ComLinear
            (0, Nothing)
            "abcbababcba"
        ~?= "[0,1,0,1,0,5,0,1,0,3,0,11,0,3,0,1,0,5,0,1,0,1,0]"

-- String: Contains an even nested palindrome with punctuation
testITLinear31 =
    "testITLinear31"
        ~: createReadableCombinator
            VarDNA
            OutLength
            ComLinear
            (4, Nothing)
            "AT\nATAT-A\tT"
        ~?= "8"

-- String: Contains an even palindrome
testITLinear32 =
    "testITLinear32"
        ~: createReadableCombinator
            VarPunctuation
            OutWord
            ComLinear
            (18, Just 18)
            "evil is a deed as I live"
        ~?= "evil is a deed as I live"

-- String: Contains an even nested palindrome with punctuation
testITLinear33 =
    "testITLinear33"
        ~: createReadableCombinator
            VarText
            OutLengths
            ComLinear
            (0, Just 0)
            "abbaccabba"
        ~?= "[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"

-- String: Contains an even palindrome with punctuation
testITLinear34 =
    "testITLinear34"
        ~: createReadableCombinator
            VarDNA
            OutWords
            ComLinear
            (14, Nothing)
            "TATG\nCGATC  GCA.TA"
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"TATG\nCGATC  GCA.TA\",\"\",\"\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Contains no palindrome, contains punctuation
testITLinear35 =
    "testITLinear35"
        ~: createReadableCombinator
            VarWord
            OutWord
            ComLinear
            (3, Nothing)
            "Thi,s is not a p-alindro'me."
        ~?= ""

-- String: Contains an odd palindrome with punctuation
testITLinear36 =
    "testITLinear36"
        ~: createReadableCombinator
            VarWord
            OutWords
            ComLinear
            (3, Nothing)
            "A simple palindrome. Simple, a (ye)?"
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"A simple palindrome. Simple, a\",\"\",\"\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Contains an even nested palindrome
testITLinear37 =
    "testITLinear37"
        ~: createReadableCombinator
            VarPunctuation
            OutWords
            ComLinear
            (0, Just 8)
            "d*eif:ied a-+bba deifie;d!"
        ~?= "[\"\",\"d\",\"\",\"\",\"\",\"\",\"\",\"d*eif:ied\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"a\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"deifie;d\",\"\",\"\",\"\",\"\",\"\",\"d\",\"\"]"

-- String: Contains no palindrome, contains punctuation
testITLinear38 =
    "testITLinear38"
        ~: createReadableCombinator
            VarPunctuation
            OutLengths
            ComLinear
            (21, Nothing)
            "Thi,s is not a p-alindro'me."
        ~?= "[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"

-- String: Contains an even palindrome
testITLinear39 =
    "testITLinear39"
        ~: createReadableCombinator
            VarDNA
            OutWord
            ComLinear
            (0, Just 0)
            "ATGCGCATTA"
        ~?= ""

-- String: Contains no palindrome, contains punctuation
testITLinear40 =
    "testITLinear40"
        ~: createReadableCombinator
            VarDNA
            OutLengths
            ComLinear
            (2, Just 5)
            "AN\nTC\tAGTC"
        ~?= "[0,0,0,0,0,0,0,0,0]"

-- String: Contains no palindrome
testITLinear41 =
    "testITLinear41"
        ~: createReadableCombinator
            VarDNA
            OutWord
            ComLinear
            (0, Just 5)
            "NNNNNNNNNN"
        ~?= ""

-- String: Contains an odd punctuation palindrome
testITLinear42 =
    "testITLinear42"
        ~: createReadableCombinator
            VarPlain
            OutLengths
            ComLinear
            (0, Just 8)
            "\nodddo!"
        ~?= "[0,1,0,1,0,1,2,5,2,1,0,1,0,1,0]"

-- String: Contains an odd palindrome
testITLinear43 =
    "testITLinear43"
        ~: createReadableCombinator
            VarPlain
            OutWords
            ComLinear
            (1, Just 4)
            "releveler"
        ~?= "[\"\",\"r\",\"\",\"e\",\"\",\"ele\",\"\",\"e\",\"\",\"\",\"\",\"e\",\"\",\"ele\",\"\",\"e\",\"\",\"r\",\"\"]"

-- String: Contains an odd nested palindrome
testITLinear44 =
    "testITLinear44"
        ~: createReadableCombinator
            VarText
            OutWord
            ComLinear
            (0, Just 12)
            "hi madam im adam bye"
        ~?= "madam im adam"

-- String: Contains an odd nested palindrome
testITLinear45 =
    "testITLinear45"
        ~: createReadableCombinator
            VarWord
            OutWords
            ComLinear
            (3, Just 3)
            "Oi! You there! You! Stop!"
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"You there! You\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Contains an odd nested palindrome
testITLinear46 =
    "testITLinear46"
        ~: createReadableCombinator
            VarPunctuation
            OutWord
            ComLinear
            (0, Just 14)
            "You! Rev Ile deliver, now!"
        ~?= "Rev Ile deliver"

-- String: Contains an odd palindrome with punctuation
testITLinear47 =
    "testITLinear47"
        ~: createReadableCombinator
            VarText
            OutLengths
            ComLinear
            (0, Just 8)
            "Revile.... Deliver!"
        ~?= "[0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0]"

-- String: Contains an even nested palindrome with punctuation
testITLinear48 =
    "testITLinear48"
        ~: createReadableCombinator
            VarPlain
            OutWords
            ComLinear
            (0, Just 4)
            "It's high noon..."
        ~?= "[\"\",\"I\",\"\",\"t\",\"\",\"'\",\"\",\"s\",\"\",\" \",\"\",\"h\",\"\",\"i\",\"\",\"g\",\"\",\"h\",\"\",\" \",\"\",\"n\",\"\",\"o\",\"noon\",\"o\",\"\",\"n\",\"\",\".\",\"..\",\"...\",\"..\",\".\",\"\"]"

-- String: Contains an odd palindrome with punctuation
testITLinear49 =
    "testITLinear49"
        ~: createReadableCombinator
            VarPlain
            OutWord
            ComLinear
            (3, Just 16)
            "!dammit_timmad!"
        ~?= "!dammit_timmad!"

-- String: Contains no punctuation palindrome
testITLinear50 =
    "testITLinear50"
        ~: createReadableCombinator
            VarPunctuation
            OutLength
            ComLinear
            (0, Just 5)
            "begone enoge"
        ~?= "0"

-- String: Contains an odd and an even palindrome
testITLinear51 =
    "testITLinear51"
        ~: createReadableCombinator
            VarText
            OutLengths
            ComLinear
            (2, Just 5)
            "madamoiselle"
        ~?= "[0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0]"

-- String: Contains an even palindrome with punctuation
testITLinear52 =
    "testITLinear52"
        ~: createReadableCombinator
            VarWord
            OutLengths
            ComLinear
            (4, Just 4)
            "Hello? my.... my .. hello!"
        ~?= "[0,0,0,0,4,0,0,0,0]"

-- String: Contains an even punctuation palindrome
testITLinear53 =
    "testITLinear53"
        ~: createReadableCombinator
            VarDNA
            OutLength
            ComLinear
            (2, Just 4)
            "\nAT  CG /AT/"
        ~?= "2"

-- String: Contains an even palindrome
testITLinear54 =
    "testITLinear54"
        ~: createReadableCombinator
            VarDNA
            OutWord
            ComLinear
            (4, Just 5)
            "CGCG"
        ~?= "CGCG"

-- String: Contains an odd nested palindrome
testITLinear55 =
    "testITLinear55"
        ~: createReadableCombinator
            VarPlain
            OutLength
            ComLinear
            (0, Just 16)
            "hi madam i madam bye"
        ~?= "15"

-- String: Contains an even nested palindrome
testITLinear56 =
    "testITLinear56"
        ~: createReadableCombinator
            VarDNA
            OutLength
            ComLinear
            (6, Just 12)
            "NNTATATATANN"
        ~?= "8"

-- String: Contains an even palindrome with punctuation
testITLinear57 =
    "testITLinear57"
        ~: createReadableCombinator
            VarPlain
            OutLength
            ComLinear
            (5, Just 10)
            "!ma;dd;am!"
        ~?= "10"

-- String: Contains no palindrome
testITLinear58 =
    "testITLinear58"
        ~: createReadableCombinator
            VarWord
            OutWord
            ComLinear
            (2, Just 4)
            "word1 word2 word3 word4"
        ~?= ""

-- String: Contains an odd palindrome
testITLinear59 =
    "testITLinear59"
        ~: createReadableCombinator
            VarText
            OutWords
            ComLinear
            (9, Just 9)
            "Releveler"
        ~?= "[\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"Releveler\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\"]"

-- String: Contains an odd punctuation palindrome
testITLinear60 =
    "testITLinear60"
        ~: createReadableCombinator
            VarPlain
            OutLength
            ComLinear
            (5, Just 0)
            "\nOdd do!"
        ~?= "0"
