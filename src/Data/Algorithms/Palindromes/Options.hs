{-# LANGUAGE PatternGuards #-}
-- Did not yet translate all options. Complete the table in dispatchFlags.
-- Default doesn't work yet
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- |
Module      :  Data.Algorithms.Palindromes.Options
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

Gives the options for flags that can be input in the command line.
Also contains the functions that are used to convert these flags to their corresponding datatype.
-}
module Data.Algorithms.Palindromes.Options where

import Data.Maybe (fromJust, isNothing)
import System.Console.GetOpt
    ( ArgDescr (..)
    , OptDescr (..)
    )

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

defaultAlgorithm :: Algorithm
defaultAlgorithm = AlgQuadratic{algGapSize = 0, algMaxError = 0}

defaultVariant :: Variant
defaultVariant = VarText

defaultOutputFormat :: OutputFormat
defaultOutputFormat = FormatText

defaultOutputFilter :: OutputFilter
defaultOutputFilter = SelectLongest

defaultMinLength :: Int
defaultMinLength = 0

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
        (OptArg parseQuadratic "[gapSize] [errors]")
        "Use the quadratic algorithm. (default) Optionally use the argument <gapSize> <errors> (default for both is 0)"
    , Option
        ['A']
        ["approximate"]
        (OptArg parseApproximate "[gapSize] [errors]")
        "Use approximate algorithm. Optionally use the argument <gapSize> <errors> (default for both is 0)"
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
    ]

-- | Detects help flag constructor.
isHelp :: Flag -> Bool
isHelp Help = True
isHelp _ = False

-- | Detects standard input flag constructor.
isStandardInput :: Flag -> Bool
isStandardInput StandardInput = True
isStandardInput _ = False

{- | Parses the optional error and gap input to a Flag. If invalid inputs are given, an
error is thrown.
-}
parseApproximate :: Maybe String -> Flag
parseApproximate str
    | isNothing str = Algorithm AlgApproximate{algGapSize = 0, algMaxError = 0}
    | null y =
        error
            ( "Invalid arguments for gap size and errors. (gap size, errors) = ("
                ++ gapSize
                ++ ", "
                ++ errors
                ++ "). s must be the last flag in a series of flags."
                ++ " Enter 2 numbers after s seperated by a '+'. For example: '-q1+2'."
            )
    | otherwise =
        Algorithm AlgApproximate{algGapSize = read gapSize, algMaxError = read errors}
  where
    (x, y) = break (== '+') $ fromJust str
    (gapSize, errors) = (x, drop 1 y)

{- | Parses the optional error input to a Flag. If invalid inputs are given, an
error is thrown.
-}
parseQuadratic :: Maybe String -> Flag
parseQuadratic str
    | isNothing str = Algorithm AlgQuadratic{algGapSize = 0, algMaxError = 0}
    | null y =
        error
            ( "Invalid arguments for gap size and errors. (gap size, errors) = ("
                ++ gapSize
                ++ ", "
                ++ errors
                ++ "). q must be the last flag in a series of flags."
                ++ " Enter 2 numbers after q seperated by a '+'. For example: '-q1+2'."
            )
    | otherwise =
        Algorithm AlgQuadratic{algGapSize = read gapSize, algMaxError = read errors}
  where
    (x, y) = break (== '+') $ fromJust str
    (gapSize, errors) = (x, drop 1 y)

{- | From all input flags, gets the algorithm setting. If more than one algorithm flag
is given, it throws an error, as this is not suppported by our program. If none are give it
uses the default option.
-}
getAlgorithm :: [Flag] -> Algorithm
getAlgorithm xs
    | null algorithmFlags = defaultAlgorithm
    | [Algorithm c] <- algorithmFlags = c
    | otherwise = error "Multiple algorithm flags detected."
  where
    isAlgorithm :: Flag -> Bool
    isAlgorithm (Algorithm _) = True
    isAlgorithm _ = False
    algorithmFlags :: [Flag]
    algorithmFlags = filter isAlgorithm xs

{- | From all input flags, gets the variant setting. If more than one variant flag is
given, it throws an error, as this is not suppported by our program. If none are give it
uses the default option.
-}
getVariant :: [Flag] -> Variant
getVariant xs
    | null variantFlags = defaultVariant
    | [Variant v] <- variantFlags = v
    | otherwise = error "Multiple variant flags detected."
  where
    isVariant :: Flag -> Bool
    isVariant (Variant _) = True
    isVariant _ = False
    variantFlags :: [Flag]
    variantFlags = filter isVariant xs

{- | From all input flags, gets the output format setting. If more than one output format
flag is given, it throws an error, as this is not suppported by our program. If none are
give it uses the default option.
-}
getOutputFormat :: [Flag] -> OutputFormat
getOutputFormat xs
    | null outputFormatFlags = defaultOutputFormat
    | [OutputFormat o] <- outputFormatFlags = o
    | otherwise = error "Multiple outputFormat flags detected."
  where
    isOutputFormat :: Flag -> Bool
    isOutputFormat (OutputFormat _) = True
    isOutputFormat _ = False
    outputFormatFlags :: [Flag]
    outputFormatFlags = filter isOutputFormat xs

{- | From all input flags, gets the output filter setting. If more than one output filter
flag is given, it throws an error, as this is not suppported by our program. If none are
give it uses the default option.
-}
getOutputFilter :: [Flag] -> OutputFilter
getOutputFilter xs
    | null outputFilterFlags = defaultOutputFilter
    | [OutputFilter o] <- outputFilterFlags = o
    | otherwise = error "Multiple outputFilter flags detected."
  where
    isOutputFilter :: Flag -> Bool
    isOutputFilter (OutputFilter _) = True
    isOutputFilter _ = False
    outputFilterFlags :: [Flag]
    outputFilterFlags = filter isOutputFilter xs

{- | From all input flags, gets the length modifier setting. If more than one length
modifier flag is given, it throws an error, as this is not suppported by our program. If
none are give it uses the default option.
-}
getMinLength :: [Flag] -> Int
getMinLength xs = minLength
  where
    isMinLength (MinLength _) = True
    isMinLength _ = False
    mins :: [Flag]
    mins = filter isMinLength xs
    minLength :: Int
    minLength
        | null mins = 2
        | [MinLength minL] <- mins = minL
        | otherwise = error "Multiple minimum lengths found."

-- | The header of the help message.
headerHelpMessage :: String
headerHelpMessage =
    "*********************\n"
        ++ "* Palindrome Finder *\n"
        ++ "* version 0.5       *\n"
        ++ "*********************\n"
        ++ "Usage: \n"
        ++ "Either give the path to a file or use the flag -i for manual input in the terminal. The following flags can be used to change settings."
