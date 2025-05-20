module QuickCheckSettings
    ( settingsList
    ) where

import Data.Algorithms.Palindromes.Finders
    ( Complexity (ComLinear, ComQuadratic)
    , OutputFormat (OutLength, OutLengths, OutWord, OutWords)
    , Variant (VarDNA, VarPlain, VarPunctuation, VarText, VarWord)
    )
import Data.Algorithms.Palindromes.Settings (Settings (..))

-- | Settings where the palindrome type works with Char
settingsList :: [Settings]
settingsList =
    [ setting1
    , setting2
    , setting3
    , setting4
    , setting5
    , setting6
    , setting7
    , setting8
    , setting9
    , setting10
    , setting11
    , setting12
    , setting13
    , setting14
    , setting15
    , setting16
    , setting17
    , setting18
    , setting19
    , setting20
    , setting21
    ]

setting1 :: Settings
setting1 =
    Settings
        ComLinear
        VarWord
        OutLength
        10
setting2 :: Settings
setting2 =
    Settings
        (ComQuadratic 10 5)
        VarPunctuation
        OutLength
        3
setting3 :: Settings
setting3 =
    Settings
        (ComQuadratic 3 1)
        VarPlain
        OutLength
        0
setting4 :: Settings
setting4 =
    Settings
        (ComQuadratic 3 5)
        VarText
        OutLength
        10
setting5 :: Settings
setting5 =
    Settings
        (ComQuadratic 10 0)
        VarPlain
        OutLength
        0
setting6 :: Settings
setting6 =
    Settings
        (ComQuadratic 0 1)
        VarWord
        OutLength
        3
setting7 :: Settings
setting7 =
    Settings
        (ComQuadratic 10 5)
        VarWord
        OutLength
        0
setting8 :: Settings
setting8 =
    Settings
        (ComQuadratic 10 0)
        VarText
        OutLength
        3
setting9 :: Settings
setting9 =
    Settings
        (ComQuadratic 3 0)
        VarWord
        OutLength
        3
setting10 :: Settings
setting10 =
    Settings
        (ComQuadratic 0 1)
        VarPunctuation
        OutLength
        0
setting11 :: Settings
setting11 =
    Settings
        (ComQuadratic 0 5)
        VarPlain
        OutLength
        3
setting12 :: Settings
setting12 =
    Settings
        (ComQuadratic 10 1)
        VarDNA
        OutLength
        10
setting13 :: Settings
setting13 =
    Settings
        (ComQuadratic 3 0)
        VarDNA
        OutLength
        3
setting14 :: Settings
setting14 =
    Settings
        (ComQuadratic 0 5)
        VarDNA
        OutLength
        0
setting15 :: Settings
setting15 =
    Settings
        ComLinear
        VarPunctuation
        OutLength
        3
setting16 :: Settings
setting16 =
    Settings
        ComLinear
        VarDNA
        OutLength
        0
setting17 :: Settings
setting17 =
    Settings
        (ComQuadratic 3 1)
        VarPunctuation
        OutLength
        10
setting18 :: Settings
setting18 =
    Settings
        (ComQuadratic 0 1)
        VarText
        OutLength
        0
setting19 :: Settings
setting19 =
    Settings
        ComLinear
        VarPlain
        OutLength
        10
setting20 :: Settings
setting20 =
    Settings
        (ComQuadratic 0 5)
        VarPlain
        OutLength
        3
setting21 :: Settings
setting21 =
    Settings
        ComLinear
        VarText
        OutLength
        10
