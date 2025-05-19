{-
A transducer is a way to go through a list, keeping track of intermediate states of some
kind and extracting zero or more elements of output based on each of these states. This
way, output can be extracted from large (or infinite) lists withput first needing to
traverse the whole list.
-}

module Data.Algorithms.Palindromes.Transducers (TransduceExtractor (..), transducel1, transducel2) where

import Data.List (scanl')

{- | Data type containing functions to extract output from states with a special function
that is called for the end state
-}
data TransduceExtractor state output = TransExtract
    { extractFunction :: state -> [output]
    , endFunction :: state -> [output]
    }

{- | Create list of states by applying the stateFunction repeatedly to the initial state
using the input list
-}
transduceMap :: (state -> input -> state) -> state -> [input] -> [state]
transduceMap stateFunction initialState inputs =
    -- scanl' puts the initial state in the list, which we do not want.
    drop 1 $ scanl' stateFunction initialState inputs

{- Use the functions from the TransduceExtractor on the list of states to generate the
output list -}
transduceExtract :: TransduceExtractor state output -> [state] -> [output]
transduceExtract _ [] = []
-- If you are at the final element, also apply the endFunction
transduceExtract (TransExtract f e) [s] = f s ++ e s
transduceExtract transExtract (s : x : t) =
    extractFunction transExtract s ++ transduceExtract transExtract (x : t)

--------------------------------------------------

-- | Get the output from a transducer with 1 output.
transducel1
    :: (state -> input -> state)
    -> TransduceExtractor state output
    -> state
    -> [input]
    -> [output]
transducel1 stateFunction transExtractor initialState input =
    transduceExtract transExtractor $ transduceMap stateFunction initialState input

-- | Get the output from a transducer with 2 outputs.
transducel2
    :: (state -> input -> state)
    -> TransduceExtractor state output0
    -> TransduceExtractor state output1
    -> state
    -> [input]
    -> ([output0], [output1])
transducel2 statef tre0 tre1 ini inp = (transduceExtract tre0 states, transduceExtract tre1 states)
  where
    states = transduceMap statef ini inp
