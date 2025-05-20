{-
A transducer is a way to go through a list, keeping track of intermediate states of some
kind and extracting zero or more elements of output based on each of these states. This
way, output can be extracted from large (or infinite) lists withput first needing to
traverse the whole list.
-}

module Data.Algorithms.Palindromes.Transducers (TransduceExtractor (..), transducel1, transducel2) where

import Data.List (scanl')

{- | Data type containing functions to extract output from states with a special function
that is called for the end state.
-}
data TransduceExtractor state output = TransExtract
    { extractFunction :: state -> [output]
    , endFunction :: state -> [output]
    }

{- | Create list of states by doing a scanl using the statefunction, the initial state and
the inputs list. The transduceExtract function can then extract information based on this
list of states.
-}
transduceMap :: (state -> input -> state) -> state -> [input] -> [state]
transduceMap stateFunction initialState inputs =
    {- scanl' puts the initial state in the list, which we do not want. So, we have to
    drop the first element. -}
    drop 1 $ scanl' stateFunction initialState inputs

{- Use the functions from the TransduceExtractor on the list of states to generate the
outputs list. -}
transduceExtract :: TransduceExtractor state output -> [state] -> [output]
transduceExtract _ [] = []
-- If you are at the final element, also apply the endFunction
transduceExtract (TransExtract{extractFunction = f, endFunction = e}) [s] = f s ++ e s
transduceExtract transExtract@TransExtract{extractFunction = f} (s : x : t) =
    f s ++ transduceExtract transExtract (x : t)

{- | Generates a list of states and extracts information from them using a single
TransduceExtractor. Because of lazy evaluation, the mapping and extraction is done
alternatingly. So, the inputs list only needs to be traversed once. This causes memory and
time efficient evaluation on large or even infinite lists.
-}
transducel1
    :: (state -> input -> state)
    -> TransduceExtractor state output
    -> state
    -> [input]
    -> [output]
transducel1 stateFunction transExtractor initialState inputs =
    transduceExtract transExtractor $ transduceMap stateFunction initialState inputs

{- | The same as transducel1, but uses two TransduceExtractors. This way, you can get two
different output lists (possibly of different lengths), using the same states for both, so
you only have to transduceMap once.
-}
transducel2
    :: (state -> input -> state)
    -> TransduceExtractor state output0
    -> TransduceExtractor state output1
    -> state
    -> [input]
    -> ([output0], [output1])
transducel2 stateFunction transExtractor0 transExtractor1 initialState inputs =
    (transduceExtract transExtractor0 states, transduceExtract transExtractor1 states)
  where
    states = transduceMap stateFunction initialState inputs
