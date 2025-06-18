{- |
Module      :  Data.Algorithms.Palindromes.Options
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

Gives the options for flags that can be inputted in the command line and contains
functions for parsing to some flags.
-}
module Data.Algorithms.Palindromes.Options (Flag (..), options) where

import Data.Maybe (fromJust, isNothing)
import System.Console.GetOpt
    ( ArgDescr (..)
    , OptDescr (..)
    )
import Text.Read (readMaybe)

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
    )

data Flag
    = Help
    | StandardInput
    | Algorithm Algorithm
    | Variant Variant
    | OutputFormat OutputFormat
    | OutputFilter OutputFilter
    | MinLength Int
    | ProgressDisabled
    deriving (Eq)

-----------------------------------------------------------------------------
-- Options
-----------------------------------------------------------------------------

{- Options describe the input flags that can be used in the command line. -}
options :: [OptDescr Flag]
options =
    [ Option
        ['h']
        ["help"]
        (NoArg Help)
        "This message"
    , Option
        ['L']
        ["linear"]
        (NoArg (Algorithm AlgLinear))
        "Use the linear algorithm"
    , Option
        ['Q']
        ["quadratic"]
        (OptArg parseQuadratic "gapSize+maxErrors")
        "Use the quadratic algorithm (default). Optional arguments gapSize and maxErrors, default is 0."
    , Option
        ['A']
        ["approximate"]
        (OptArg parseApproximate "gapSize+maxErrors")
        "Use approximate algorithm. Optional arguments gapSize and maxErrors, default is 0."
    , Option
        ['R']
        ["plain", "regular"]
        (NoArg (Variant VarPlain))
        "plain (r for regular) palindrome"
    , Option
        []
        ["text"]
        (NoArg (Variant VarText))
        "Palindrome ignoring case, spacing and punctuation (default)"
    , Option
        ['P']
        ["punctuation"]
        (NoArg (Variant VarPunctuation))
        "Palindrome surrounded by punctuation (if any)"
    , Option
        ['W']
        ["word"]
        (NoArg (Variant VarWord))
        "Word palindrome"
    , Option
        ['D']
        ["dna"]
        (NoArg (Variant VarDNA))
        "DNA palindrome"
    , Option
        []
        ["textformat"]
        (NoArg (OutputFormat FormatText))
        "Output the text of the palindromes (default)"
    , Option
        ['l']
        ["length"]
        (NoArg (OutputFormat FormatLength))
        "Output the length of the palindromes"
    , Option
        ['r']
        ["range"]
        (NoArg (OutputFormat FormatRange))
        "Output the range of the palindromes"
    , Option
        ['d']
        ["details"]
        (NoArg (OutputFormat FormatAllDetails))
        "Output the text, range and length of the palindromes"
    , Option
        []
        ["longest"]
        (NoArg (OutputFilter SelectLongest))
        "Select only the longest palindromes, can be multiple of same length (default)"
    , Option
        ['a']
        ["all"]
        (NoArg (OutputFilter SelectAll))
        "Select all maximal palindromes"
    , Option
        ['c']
        ["center"]
        (ReqArg (OutputFilter . SelectAt . (read :: String -> Int)) "arg")
        "Find only the palindromes around the center [arg]"
    , Option
        ['m']
        ["minlength", "min"]
        (ReqArg (MinLength . (read :: String -> Int)) "arg")
        "Maximal palindromes of length at least [arg]. A value larger than 1 is strongly recommended to avoid trivial palindromes."
    , Option
        ['i']
        ["input"]
        (NoArg StandardInput)
        "Read input from standard input"
    , Option
        ['p']
        ["disable-progress"]
        (NoArg ProgressDisabled)
        "Disable the progress bar, this can help boost preformance."
    ]

{- | Parses the optional error and gap input to a Flag. If invalid inputs are given, an
error is thrown.
-}
parseApproximate :: Maybe String -> Flag
parseApproximate str
    | isNothing str = Algorithm AlgApproximate{algGapSize = 0, algMaxErrors = 0}
    | null y =
        error $
            "Invalid arguments for -A/--approximate: \'"
                ++ fromJust str
                ++ "\'. Enter arguments as 2 numbers after seperated by a '+'. For example: '-A1+2'."
    | y == "+" =
        error $
            "Invalid arguments for -A/--approximate: \'"
                ++ fromJust str
                ++ "\'. Please enter second argument"
    | isNothing gapSize || isNothing maxErrors =
        error $
            "Invalid arguments for -A/--approximate: \'"
                ++ fromJust str
                ++ "\'. Arguments must be integers."
    | otherwise =
        Algorithm AlgApproximate{algGapSize = fromJust gapSize, algMaxErrors = fromJust maxErrors}
  where
    (x, y) = break (== '+') $ fromJust str
    (gapSize, maxErrors) = (readMaybe x, readMaybe (drop 1 y))

{- | Parses the optional error input to a Flag. If invalid inputs are given, an
error is thrown.
-}
parseQuadratic :: Maybe String -> Flag
parseQuadratic str
    | isNothing str = Algorithm AlgQuadratic{algGapSize = 0, algMaxErrors = 0}
    | null y =
        error $
            "Invalid arguments for -Q/--quadratic: \'"
                ++ fromJust str
                ++ "\'. Enter arguments as 2 numbers after seperated by a '+'. For example: '-Q1+2'."
    | y == "+" =
        error $
            "Invalid arguments for -Q/--quadratic: \'"
                ++ fromJust str
                ++ "\'. Please enter second argument"
    | isNothing gapSize || isNothing maxErrors =
        error $
            "Invalid arguments for -Q/--quadratic: \'"
                ++ fromJust str
                ++ "\'. Arguments must be integers."
    | otherwise =
        Algorithm AlgQuadratic{algGapSize = fromJust gapSize, algMaxErrors = fromJust maxErrors}
  where
    (x, y) = break (== '+') $ fromJust str
    (gapSize, maxErrors) = (readMaybe x, readMaybe (drop 1 y))
