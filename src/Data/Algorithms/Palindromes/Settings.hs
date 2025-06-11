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
    ( Settings (..)
    , getSettings
    , defaultSettings
    , handleFlags
    )
where

import Data.List (intercalate)
import System.Console.GetOpt (usageInfo)

import Data.Algorithms.Palindromes.Finders
    ( Complexity (..)
    , OutputFormat (..)
    , Variant (..)
    , formatPalindromes
    )
import Data.Algorithms.Palindromes.Options
    ( Flag
    , defaultComplexity
    , defaultMinLength
    , defaultOutputFormat
    , defaultVariant
    , getComplexity
    , getMinLength
    , getOutputFormat
    , getVariant
    , headerHelpMessage
    , isHelp
    , isStandardInput
    , options
    )
import Data.Algorithms.Palindromes.Streaming (findPalindromesWithProgressBar)

-- | Data type with all the settings required for running algorithm.
data Settings = Settings
    { complexity :: Complexity
    , variant :: Variant
    , outputFormat :: OutputFormat
    , minLength :: Int
    }

instance Show Settings where
    show settings = intercalate ", " settingsSpecs
      where
        settingsSpecs =
            [ show (complexity settings)
            , show (variant settings)
            , show (outputFormat settings)
            , show (minLength settings)
            ]

-- | If no flags are given to modify settings default settings are used
defaultSettings :: Settings
defaultSettings =
    Settings
        { complexity = defaultComplexity
        , variant = defaultVariant
        , outputFormat = defaultOutputFormat
        , minLength = defaultMinLength
        }

-- | Gets settings from the list of input flags. Uses default if no flags are given.
getSettings :: [Flag] -> Settings
getSettings flags =
    Settings
        { complexity = getComplexity flags
        , variant = getVariant flags
        , outputFormat = getOutputFormat flags
        , minLength = getMinLength flags
        }

-- | Retrieves all palindromes matching the settings using a progress bar and then formats them to a string
applySettingsToFinder :: Settings -> (String -> IO String)
applySettingsToFinder (Settings{complexity = c, variant = v, outputFormat = o, minLength = l}) s = do
    pals <- findPalindromesWithProgressBar v c l s
    return (formatPalindromes o pals)

{- | Based on input flags, gets a tuple with a function that directly encapsulates
everything from the input string to the output string. Also encodes whether input string
is from a file or standard input.
-}
handleFlags
    :: [Flag]
    -> Bool -- hasFiles
    -> ( String -> IO String -- function from input to output
       , Bool -- if input is standard input
       )
handleFlags flags hasFiles =
    ( if any isHelp flags || (null flags && not hasFiles)
        then const $ return (usageInfo headerHelpMessage options)
        else applySettingsToFinder (getSettings flags)
    , any isStandardInput flags
    )
