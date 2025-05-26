{- |
Module      :  Data.Algorithms.Palindromes.Palindrome
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

Incremental state generation with output extraction is a way to go through a list, keeping track of intermediate states of some
kind and extracting zero or more elements of output based on each of these states. This
way, output can be extracted from large (or infinite) lists withput first needing to
traverse the whole list.
-}
module Data.Algorithms.Palindromes.IncrementalStateGenerationWithOutputExtraction
    ( Extractor (..)
    , incrementalStateGenerationWithOutputExtraction
    , incrementalStateGenerationWithTwoOutputExtractions
    ) where

import Data.List (scanl')

{- | Data type containing functions to extract output from states and a special function
that is called for the end state.
-}
data Extractor state output = Extractor
    { extractFunction :: state -> [output]
    , endFunction :: state -> [output]
    }

{- | Create list of states by doing a scanl using the statefunction, the initial state and
the inputs list. The Extractor can then extract information based on this list of states.
-}
generateIncrementalStates :: (state -> input -> state) -> state -> [input] -> [state]
generateIncrementalStates stateFunction initialState inputs =
    {- scanl' puts the initial state in the list, which we do not want. So, we have to
    drop the first element. -}
    drop 1 $ scanl' stateFunction initialState inputs

{- Use the functions from the Extractor on the list of states to generate the outputs
list. This function is semantically equivalent to ```concatMap extractFunction states ++
endFunction (last states)```, but this implementation is more efficient because it does not
use 'last'. -}
extract :: Extractor state output -> [state] -> [output]
extract _ [] = []
-- If you are at the final element, also apply the endFunction
extract (Extractor{extractFunction = f, endFunction = e}) [s] = f s ++ e s
extract extractor@Extractor{extractFunction = f} (s : x : t) =
    f s
        ++ extract
            extractor
            (x : t)

{- | Generates a list of states and extracts information from them using a single
Extractor. Because of lazy evaluation, the mapping and extraction is done alternatingly.
So, the inputs list only needs to be traversed once. This causes memory and time efficient
evaluation on large or even infinite lists.
-}
incrementalStateGenerationWithOutputExtraction
    :: (state -> input -> state)
    -> Extractor state output
    -> state
    -> [input]
    -> [output]
incrementalStateGenerationWithOutputExtraction stateFunction extractor initialState inputs =
    extract extractor $ generateIncrementalStates stateFunction initialState inputs

{- | The same as stateGenerationWithOutputExtraction, but uses two Extractors. This way,
you can get two different output lists (possibly of different lengths), using the same
states for both, so you only have to generate states once.
-}
incrementalStateGenerationWithTwoOutputExtractions
    :: (state -> input -> state)
    -> Extractor state output0
    -> Extractor state output1
    -> state
    -> [input]
    -> ([output0], [output1])
incrementalStateGenerationWithTwoOutputExtractions stateFunction extractor0 extractor1 initialState inputs =
    (extract extractor0 states, extract extractor1 states)
  where
    states = generateIncrementalStates stateFunction initialState inputs
