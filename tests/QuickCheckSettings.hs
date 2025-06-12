module QuickCheckSettings
    ( settingsList
    ) where

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
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
    , setting22
    , setting23
    , setting24
    , setting25
    , setting26
    , setting27
    , setting28
    , setting29
    , setting30
    , setting31
    , setting32
    , setting33
    , setting34
    , setting35
    , setting36
    , setting37
    , setting38
    ]

setting1 :: Settings
setting1 =
    Settings
        ComLinear
        VarWord
        FormatLength
        SelectLongest
        10
setting2 :: Settings
setting2 =
    Settings
        (ComQuadratic 10 5)
        VarPunctuation
        FormatLength
        SelectLongest
        3
setting3 :: Settings
setting3 =
    Settings
        (ComQuadratic 3 1)
        VarPlain
        FormatLength
        SelectLongest
        0
setting4 :: Settings
setting4 =
    Settings
        (ComQuadratic 3 5)
        VarText
        FormatLength
        SelectLongest
        10
setting5 :: Settings
setting5 =
    Settings
        (ComQuadratic 10 0)
        VarPlain
        FormatLength
        SelectLongest
        0
setting6 :: Settings
setting6 =
    Settings
        (ComQuadratic 0 1)
        VarWord
        FormatLength
        SelectLongest
        3
setting7 :: Settings
setting7 =
    Settings
        (ComQuadratic 10 5)
        VarWord
        FormatLength
        SelectLongest
        0
setting8 :: Settings
setting8 =
    Settings
        (ComQuadratic 10 0)
        VarText
        FormatLength
        SelectLongest
        3
setting9 :: Settings
setting9 =
    Settings
        (ComQuadratic 3 0)
        VarWord
        FormatLength
        SelectLongest
        3
setting10 :: Settings
setting10 =
    Settings
        (ComQuadratic 0 1)
        VarPunctuation
        FormatLength
        SelectLongest
        0
setting11 :: Settings
setting11 =
    Settings
        (ComQuadratic 0 5)
        VarPlain
        FormatLength
        SelectLongest
        3
setting12 :: Settings
setting12 =
    Settings
        (ComQuadratic 10 1)
        VarDNA
        FormatLength
        SelectLongest
        10
setting13 :: Settings
setting13 =
    Settings
        (ComQuadratic 3 0)
        VarDNA
        FormatLength
        SelectLongest
        3
setting14 :: Settings
setting14 =
    Settings
        (ComQuadratic 0 5)
        VarDNA
        FormatLength
        SelectLongest
        0
setting15 :: Settings
setting15 =
    Settings
        ComLinear
        VarPunctuation
        FormatLength
        SelectLongest
        3
setting16 :: Settings
setting16 =
    Settings
        ComLinear
        VarDNA
        FormatLength
        SelectLongest
        0
setting17 :: Settings
setting17 =
    Settings
        (ComQuadratic 3 1)
        VarPunctuation
        FormatLength
        SelectLongest
        10
setting18 :: Settings
setting18 =
    Settings
        (ComQuadratic 0 1)
        VarText
        FormatLength
        SelectLongest
        0
setting19 :: Settings
setting19 =
    Settings
        ComLinear
        VarPlain
        FormatLength
        SelectLongest
        10
setting20 :: Settings
setting20 =
    Settings
        (ComQuadratic 0 5)
        VarPlain
        FormatLength
        SelectLongest
        3
setting21 :: Settings
setting21 =
    Settings
        ComLinear
        VarText
        FormatLength
        SelectLongest
        10
setting22 =
    Settings
        (ComInsertionDeletion 10 5)
        VarPunctuation
        FormatLength
        SelectLongest
        3
setting23 :: Settings
setting23 =
    Settings
        (ComInsertionDeletion 3 1)
        VarPlain
        FormatLength
        SelectLongest
        0
setting24 :: Settings
setting24 =
    Settings
        (ComInsertionDeletion 3 5)
        VarText
        FormatLength
        SelectLongest
        10
setting25 :: Settings
setting25 =
    Settings
        (ComInsertionDeletion 10 0)
        VarPlain
        FormatLength
        SelectLongest
        0
setting26 :: Settings
setting26 =
    Settings
        (ComInsertionDeletion 0 1)
        VarWord
        FormatLength
        SelectLongest
        3
setting27 :: Settings
setting27 =
    Settings
        (ComInsertionDeletion 10 5)
        VarWord
        FormatLength
        SelectLongest
        0
setting28 :: Settings
setting28 =
    Settings
        (ComInsertionDeletion 10 0)
        VarText
        FormatLength
        SelectLongest
        3
setting29 :: Settings
setting29 =
    Settings
        (ComInsertionDeletion 3 0)
        VarWord
        FormatLength
        SelectLongest
        3
setting30 :: Settings
setting30 =
    Settings
        (ComInsertionDeletion 0 1)
        VarPunctuation
        FormatLength
        SelectLongest
        0
setting31 :: Settings
setting31 =
    Settings
        (ComInsertionDeletion 0 5)
        VarPlain
        FormatLength
        SelectLongest
        3
setting32 :: Settings
setting32 =
    Settings
        (ComInsertionDeletion 10 1)
        VarDNA
        FormatLength
        SelectLongest
        10
setting33 :: Settings
setting33 =
    Settings
        (ComInsertionDeletion 3 0)
        VarDNA
        FormatLength
        SelectLongest
        3
setting34 :: Settings
setting34 =
    Settings
        (ComInsertionDeletion 0 5)
        VarDNA
        FormatLength
        SelectLongest
        0
setting35 :: Settings
setting35 =
    Settings
        (ComInsertionDeletion 3 1)
        VarPunctuation
        FormatLength
        SelectLongest
        10
setting36 :: Settings
setting36 =
    Settings
        (ComInsertionDeletion 0 1)
        VarText
        FormatLength
        SelectLongest
        0
setting37 :: Settings
setting37 =
    Settings
        (ComInsertionDeletion 0 5)
        VarPlain
        FormatLength
        SelectLongest
        3
setting38 :: Settings
setting38 =
    Settings
        (ComInsertionDeletion 2 3)
        VarDNA
        FormatLength
        SelectLongest
        0
