{- |
Module      :  Data.Algorithms.Palindromes.Settings
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

Describes the settings for the palindrome finder functions.
Functions that are used to get settings,
and therefore also functions that apply settings to go from input to output are also described here.
-}
module Data.Algorithms.Palindromes.Settings
    ( getSettings
    , getOutput
    , defaultSettings
    , handleFlags
    )
where

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , LengthMod
    , OutputFormat (..)
    , Variant (..)
    , findPalindromesFormatted
    )
import Data.Algorithms.Palindromes.Options
    ( Flag
    , defaultComplexity
    , defaultLengthMod
    , defaultOutputFormat
    , defaultVariant
    , getComplexity
    , getLengthMod
    , getOutputFormat
    , getVariant
    , headerHelpMessage
    , isHelp
    , isStandardInput
    , options
    )
import System.Console.GetOpt (usageInfo)

-- | Data type with all the settings required for running algorithm.
data Settings = Settings
    { complexity :: Complexity
    , variant :: Variant
    , outputFormat :: OutputFormat
    , lengthMod :: LengthMod
    }

-- | If no flags are given to modify settings default settings are used
defaultSettings :: Settings
defaultSettings =
    Settings
        { complexity = defaultComplexity
        , variant = defaultVariant
        , outputFormat = defaultOutputFormat
        , lengthMod = defaultLengthMod
        }

-- | Gets settings from the list of input flags. Uses default if no flags are given.
getSettings :: [Flag] -> Settings
getSettings flags =
    Settings
        { complexity = getComplexity flags
        , variant = getVariant flags
        , outputFormat = getOutputFormat flags
        , lengthMod = getLengthMod flags
        }

-- | should be the same as findPalindromesFormatted, but using the settings datatype.
getOutput :: Settings -> (String -> String)
getOutput (Settings{complexity = c, variant = v, outputFormat = o, lengthMod = l}) = findPalindromesFormatted v o c l

{- | Based on input flags gets a tuple with a function that directly encapsuling everything from the input string to the output string.
Also encodes whether inputstring is from a file or standard input.
-}
handleFlags
    :: [Flag]
    -> ( String -> String -- function from input to output
       , Bool -- if input is standard input
       )
handleFlags flags =
    ( if any isHelp flags || null flags
        then (\_ -> usageInfo headerHelpMessage options)
        else getOutput (getSettings flags)
    , any isStandardInput flags
    )
