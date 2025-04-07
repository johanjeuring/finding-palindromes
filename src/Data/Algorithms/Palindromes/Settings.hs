-----------------------------------------------------------------------------
--
-- Module      :  Data.Algorithms.Palindromes.Settings
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
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

-- | Data type with all settings required for running algorithm
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

getSettings :: [Flag] -> Settings
getSettings flags =
    Settings
        { complexity = getComplexity flags
        , variant = getVariant flags
        , outputFormat = getOutputFormat flags
        , lengthMod = getLengthMod flags
        }

-- | should be the same as findPalindromesFormatted, but now with settings as input type instead of four different fields.
getOutput :: Settings -> (String -> String)
getOutput (Settings{complexity = c, variant = v, outputFormat = o, lengthMod = l}) = findPalindromesFormatted v o c l

{- | Does what handle options currently does. Except that it getsSetting and the output instead of a lot of maybe flags into dispatchflags.
| TODO: find out whethere we can separate the bool from this function as it is not pretty.
-}
handleFlags
    :: [Flag]
    -> ( String -> String -- function from input to output
       , Bool -- if input is standard input TODO: find out what standard input is and how it works...
       )
handleFlags flags =
    ( if any isHelp flags || null flags
        then (\_ -> usageInfo headerHelpMessage options)
        else getOutput (getSettings flags)
    , any isStandardInput flags
    )
