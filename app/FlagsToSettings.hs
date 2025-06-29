{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}

{- |
Module      :  FlagsToSettings
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

This module contains functions that are used to get settings from the flags.
-}
module FlagsToSettings
    ( getSettings
    , defaultSettings
    )
where

import Data.Algorithms.Palindromes.Finders
    ( Algorithm (..)
    , OutputFilter (..)
    , OutputFormat (..)
    , Variant (..)
    )
import Data.Algorithms.Palindromes.Settings (Settings (..))
import Options (Flag (..))

-- | If no flags are given to modify settings default settings are used
defaultSettings :: Settings
defaultSettings =
    Settings
        { algorithm = defaultAlgorithm
        , variant = defaultVariant
        , outputFormat = defaultOutputFormat
        , outputFilter = defaultOutputFilter
        , minLength = defaultMinLength
        }

defaultAlgorithm :: Algorithm
defaultAlgorithm = AlgQuadratic{algGapSize = 0, algMaxErrors = 0}

defaultVariant :: Variant
defaultVariant = VarText

defaultOutputFormat :: OutputFormat
defaultOutputFormat = FormatText

defaultOutputFilter :: OutputFilter
defaultOutputFilter = SelectLongest

defaultMinLength :: Int
defaultMinLength = 2

{- | Gets settings from the list of input flags. Uses default if no flags are given.
Evaluates all Settings records to WHNF using bang patterns to ensure all flags are valid
before calculation begins
-}
getSettings :: [Flag] -> Settings
getSettings flags =
    Settings
        { algorithm = alg
        , variant = var
        , outputFormat = fmt
        , outputFilter = flt
        , minLength = minL
        }
  where
    !alg = getAlgorithm flags
    !var = getVariant flags
    !fmt = getOutputFormat flags
    !flt = getOutputFilter flags
    !minL = getMinLength flags

    {- From all input flags, gets the algorithm setting. If more than one algorithm flag
    is given, it throws an error, as this is not suppported by our program. If none are give it
    uses the default option. -}
    getAlgorithm :: [Flag] -> Algorithm
    getAlgorithm xs
        | null algorithmFlags = defaultAlgorithm
        | [Algorithm a] <- algorithmFlags = a
        | otherwise = error "Multiple algorithm flags detected."
      where
        isAlgorithm :: Flag -> Bool
        isAlgorithm (Algorithm _) = True
        isAlgorithm _ = False
        algorithmFlags :: [Flag]
        algorithmFlags = filter isAlgorithm xs

    {- From all input flags, gets the variant setting. If more than one variant flag is
    given, it throws an error, as this is not suppported by our program. If none are give it
    uses the default option. -}
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

    {- From all input flags, gets the output format setting. If more than one output format
    flag is given, it throws an error, as this is not suppported by our program. If none are
    give it uses the default option. -}
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

    {- From all input flags, gets the output filter setting. If more than one output filter
    flag is given, it throws an error, as this is not suppported by our program. If none are
    give it uses the default option. -}
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

    {- From all input flags, gets the length modifier setting. If more than one length
    modifier flag is given, it throws an error, as this is not suppported by our program. If
    none are give it uses the default option. -}
    getMinLength :: [Flag] -> Int
    getMinLength xs
        | null minLengthFlags = defaultMinLength
        | [MinLength m] <- minLengthFlags = m
        | otherwise = error "Multiple minimum lengths found."
      where
        isMinLength (MinLength _) = True
        isMinLength _ = False
        minLengthFlags :: [Flag]
        minLengthFlags = filter isMinLength xs
