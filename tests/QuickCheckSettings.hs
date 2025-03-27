module QuickCheckSettings where

import Data.Algorithms.Palindromes.Combinators
    ( Complexity (ComLinear, ComQuadratic)
    , Output (OutLength, OutLengths, OutWord, OutWords)
    , Variant (VarDNA, VarPlain, VarText, VarWord)
    )
import Data.Algorithms.Palindromes.Settings (Settings (..))

settingsList :: [Settings]
settingsList =
    [ -- setting1
      setting2
    , setting3
    , setting4
    , -- , setting5
      setting6
    , setting7
    , setting8
    , setting9
    , -- , setting10
      setting11
    , setting12
    , setting13
    , setting14
    , setting15
    , setting16
    , setting17
    ]

setting1 :: Settings
setting1 =
    Settings
        ComLinear
        VarDNA
        OutLength
        (10, Just 100)
setting2 :: Settings
setting2 =
    Settings
        (ComQuadratic 3 5)
        VarPlain
        OutLength
        (3, Nothing)
setting3 :: Settings
setting3 =
    Settings
        (ComQuadratic 10 0)
        VarWord
        OutLength
        (0, Just 5)
setting4 :: Settings
setting4 =
    Settings
        (ComQuadratic 0 1)
        VarWord
        OutLength
        (3, Just 100)
setting5 :: Settings
setting5 =
    Settings
        (ComQuadratic 3 5)
        VarDNA
        OutLength
        (0, Just 5)
setting6 :: Settings
setting6 =
    Settings
        (ComQuadratic 10 5)
        VarText
        OutLength
        (10, Just 100)
setting7 :: Settings
setting7 =
    Settings
        (ComQuadratic 3 1)
        VarPlain
        OutLength
        (0, Just 100)
setting8 :: Settings
setting8 =
    Settings
        (ComQuadratic 0 1)
        VarText
        OutLength
        (0, Nothing)
setting9 :: Settings
setting9 =
    Settings
        (ComQuadratic 0 1)
        VarPlain
        OutLength
        (0, Just 5)
setting10 :: Settings
setting10 =
    Settings
        (ComQuadratic 10 1)
        VarDNA
        OutLength
        (3, Nothing)
setting11 :: Settings
setting11 =
    Settings
        ComLinear
        VarPlain
        OutLength
        (10, Nothing)
setting12 :: Settings
setting12 =
    Settings
        (ComQuadratic 3 0)
        VarText
        OutLength
        (3, Just 5)
setting13 :: Settings
setting13 =
    Settings
        ComLinear
        VarWord
        OutLength
        (3, Just 5)
setting14 :: Settings
setting14 =
    Settings
        (ComQuadratic 3 5)
        VarWord
        OutLength
        (10, Nothing)
setting15 :: Settings
setting15 =
    Settings
        ComLinear
        VarText
        OutLength
        (0, Just 100)
setting16 :: Settings
setting16 =
    Settings
        (ComQuadratic 0 5)
        VarText
        OutLength
        (0, Just 5)
setting17 :: Settings
setting17 =
    Settings
        (ComQuadratic 10 5)
        VarPlain
        OutLength
        (10, Nothing)
