module QuickCheckSettings
    ( settingsList
    ) where

import Data.Algorithms.Palindromes.Finders
    ( Complexity (ComInsertionDeletion, ComLinear, ComQuadratic)
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
    ]

setting1 :: Settings
setting1 =
    Settings
        ComLinear
        VarWord
        OutLength
        (10, Nothing)
setting2 :: Settings
setting2 =
    Settings
        (ComQuadratic 10 5)
        VarPunctuation
        OutLength
        (3, Nothing)
setting3 :: Settings
setting3 =
    Settings
        (ComQuadratic 3 1)
        VarPlain
        OutLength
        (0, Just 5)
setting4 :: Settings
setting4 =
    Settings
        (ComQuadratic 3 5)
        VarText
        OutLength
        (10, Just 100)
setting5 :: Settings
setting5 =
    Settings
        (ComQuadratic 10 0)
        VarPlain
        OutLength
        (0, Nothing)
setting6 :: Settings
setting6 =
    Settings
        (ComQuadratic 0 1)
        VarWord
        OutLength
        (3, Just 5)
setting7 :: Settings
setting7 =
    Settings
        (ComQuadratic 10 5)
        VarWord
        OutLength
        (0, Just 100)
setting8 :: Settings
setting8 =
    Settings
        (ComQuadratic 10 0)
        VarText
        OutLength
        (3, Just 5)
setting9 :: Settings
setting9 =
    Settings
        (ComQuadratic 3 0)
        VarWord
        OutLength
        (3, Nothing)
setting10 :: Settings
setting10 =
    Settings
        (ComQuadratic 0 1)
        VarPunctuation
        OutLength
        (0, Just 100)
setting11 :: Settings
setting11 =
    Settings
        (ComQuadratic 0 5)
        VarPlain
        OutLength
        (3, Just 100)
setting12 :: Settings
setting12 =
    Settings
        (ComQuadratic 10 1)
        VarDNA
        OutLength
        (10, Nothing)
setting13 :: Settings
setting13 =
    Settings
        (ComQuadratic 3 0)
        VarDNA
        OutLength
        (3, Just 100)
setting14 :: Settings
setting14 =
    Settings
        (ComQuadratic 0 5)
        VarDNA
        OutLength
        (0, Nothing)
setting15 :: Settings
setting15 =
    Settings
        ComLinear
        VarPunctuation
        OutLength
        (3, Just 5)
setting16 :: Settings
setting16 =
    Settings
        ComLinear
        VarDNA
        OutLength
        (0, Just 100)
setting17 :: Settings
setting17 =
    Settings
        (ComQuadratic 3 1)
        VarPunctuation
        OutLength
        (10, Nothing)
setting18 :: Settings
setting18 =
    Settings
        (ComQuadratic 0 1)
        VarText
        OutLength
        (0, Nothing)
setting19 :: Settings
setting19 =
    Settings
        ComLinear
        VarPlain
        OutLength
        (10, Nothing)
setting20 :: Settings
setting20 =
    Settings
        (ComQuadratic 0 5)
        VarPlain
        OutLength
        (3, Just 5)
setting21 :: Settings
setting21 =
    Settings
        ComLinear
        VarText
        OutLength
        (10, Nothing)
setting22 =
    Settings
        (ComInsertionDeletion 10 5)
        VarPunctuation
        OutLength
        (3, Nothing)
setting23 :: Settings
setting23 =
    Settings
        (ComInsertionDeletion 3 1)
        VarPlain
        OutLength
        (0, Just 5)
setting24 :: Settings
setting24 =
    Settings
        (ComInsertionDeletion 3 5)
        VarText
        OutLength
        (10, Just 100)
setting25 :: Settings
setting25 =
    Settings
        (ComInsertionDeletion 10 0)
        VarPlain
        OutLength
        (0, Nothing)
setting26 :: Settings
setting26 =
    Settings
        (ComInsertionDeletion 0 1)
        VarWord
        OutLength
        (3, Just 5)
setting27 :: Settings
setting27 =
    Settings
        (ComInsertionDeletion 10 5)
        VarWord
        OutLength
        (0, Just 100)
setting28 :: Settings
setting28 =
    Settings
        (ComInsertionDeletion 10 0)
        VarText
        OutLength
        (3, Just 5)
setting29 :: Settings
setting29 =
    Settings
        (ComInsertionDeletion 3 0)
        VarWord
        OutLength
        (3, Nothing)
setting30 :: Settings
setting30 =
    Settings
        (ComInsertionDeletion 0 1)
        VarPunctuation
        OutLength
        (0, Just 100)
setting31 :: Settings
setting31 =
    Settings
        (ComInsertionDeletion 0 5)
        VarPlain
        OutLength
        (3, Just 100)
setting32 :: Settings
setting32 =
    Settings
        (ComInsertionDeletion 10 1)
        VarDNA
        OutLength
        (10, Nothing)
setting33 :: Settings
setting33 =
    Settings
        (ComInsertionDeletion 3 0)
        VarDNA
        OutLength
        (3, Just 100)
setting34 :: Settings
setting34 =
    Settings
        (ComInsertionDeletion 0 5)
        VarDNA
        OutLength
        (0, Nothing)
setting35 :: Settings
setting35 =
    Settings
        (ComInsertionDeletion 3 1)
        VarPunctuation
        OutLength
        (10, Nothing)
setting36 :: Settings
setting36 =
    Settings
        (ComInsertionDeletion 0 1)
        VarText
        OutLength
        (0, Nothing)
setting37 :: Settings
setting37 =
    Settings
        (ComInsertionDeletion 0 5)
        VarPlain
        OutLength
        (3, Just 5)
