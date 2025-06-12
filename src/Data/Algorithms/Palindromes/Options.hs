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

import qualified Data.Algorithms.Palindromes.Finders as F

data Flag
    = Help
    | StandardInput
    | Complexity F.Complexity
    | Variant F.Variant
    | OutputFormat F.OutputFormat
    | MinLength Int

defaultComplexity :: F.Complexity
defaultComplexity = F.ComQuadratic{F.gapSize = 0, F.maxError = 0}

defaultVariant :: F.Variant
defaultVariant = F.VarText

defaultOutputFormat :: F.OutputFormat
defaultOutputFormat = F.OutWord

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
        (NoArg (Complexity F.ComLinear))
        "Use the linear algorithm"
    , Option
        ['Q']
        ["quadratic"]
        (OptArg parseQuadratic "[gapSize] [errors]")
        "Use the quadratic algorithm. (default) Optionally use the argument <gapSize> <errors> (default for both is 0)"
    , Option
        ['A']
        ["approximate"]
        (OptArg parseInsertionDeletion "[gapSize] [errors]")
        "Use Insertion Deletion algorithm. Optionally use the argument <gapSize> <errors> (default for both is 0)"
    , Option
        ['r']
        ["plain", "regular"]
        (NoArg (Variant F.VarPlain))
        "plain (r for regular) palindrome"
    , Option
        ['t']
        ["text"]
        (NoArg (Variant F.VarText))
        "Palindrome ignoring case, spacing and punctuation (default)"
    , Option
        ['p']
        ["punctuation"]
        (NoArg (Variant F.VarPunctuation))
        "Palindrome surrounded by punctuation (if any)"
    , Option
        ['w']
        ["word"]
        (NoArg (Variant F.VarWord))
        "Word palindrome"
    , Option
        ['d']
        ["dna"]
        (NoArg (Variant F.VarDNA))
        "DNA palindrome"
    , Option
        ['l']
        ["longest"]
        (NoArg (OutputFormat F.OutWord))
        "All longest palindromes of same size (default)"
    , Option
        ['e']
        ["length"]
        (NoArg (OutputFormat F.OutLength))
        "Length of the longest palindrome"
    , Option
        ['a']
        ["all"]
        (NoArg (OutputFormat F.OutWords))
        "All maximal palindromes"
    , Option
        ['n']
        ["lengths"]
        (NoArg (OutputFormat F.OutLengths))
        "Length of the maximal palindrome around each position in the input"
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
        ['x']
        ["extend"]
        (ReqArg (OutputFormat . F.OutLengthAt . (read :: String -> Int)) "arg")
        "Extend a palindrome around center [arg]"
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
parseInsertionDeletion :: Maybe String -> Flag
parseInsertionDeletion str
    | isNothing str = Complexity F.ComInsertionDeletion{F.gapSizeID = 0, F.maxIDError = 0}
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
        Complexity F.ComInsertionDeletion{F.gapSizeID = read gapSize, F.maxIDError = read errors}
  where
    (x, y) = break (== '+') $ fromJust str
    (gapSize, errors) = (x, drop 1 y)

{- | Parses the optional error input to a Flag. If invalid inputs are given, an
error is thrown.
-}
parseQuadratic :: Maybe String -> Flag
parseQuadratic str
    | isNothing str = Complexity F.ComQuadratic{F.gapSize = 0, F.maxError = 0}
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
        Complexity F.ComQuadratic{F.gapSize = read gapSize, F.maxError = read errors}
  where
    (x, y) = break (== '+') $ fromJust str
    (gapSize, errors) = (x, drop 1 y)

{- | From all input flags, gets the complexity setting. If more than one complexity flag
is given, it throws an error, as this is not suppported by our program. If none are give it
uses the default option.
-}
getComplexity :: [Flag] -> F.Complexity
getComplexity xs
    | null complexityFlags = defaultComplexity
    | [Complexity c] <- complexityFlags = c
    | otherwise = error "Multiple complexity flags detected."
  where
    isComplexity :: Flag -> Bool
    isComplexity (Complexity _) = True
    isComplexity _ = False
    complexityFlags :: [Flag]
    complexityFlags = filter isComplexity xs

{- | From all input flags, gets the variant setting. If more than one variant flag is
given, it throws an error, as this is not suppported by our program. If none are give it
uses the default option.
-}
getVariant :: [Flag] -> F.Variant
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
getOutputFormat :: [Flag] -> F.OutputFormat
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
