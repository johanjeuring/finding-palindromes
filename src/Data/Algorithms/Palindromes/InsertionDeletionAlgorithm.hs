{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

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

The insertion deletion algorithm for maximal gapped approximate palindromes with a maximum
number of insertion, deletion and substitution errors.
-}
module Data.Algorithms.Palindromes.InsertionDeletionAlgorithm (Cell (..), insertionDeletionAlgorithm, sparsify) where

import Data.Algorithms.Palindromes.IncrementalStateGenerationWithOutputExtraction
    ( Extractor (..)
    , incrementalStateGenerationWithOutputExtraction
    , incrementalStateGenerationWithTwoOutputExtractions
    )
import Data.Algorithms.Palindromes.PalEq (PalEq, (=:=))

import qualified Data.Vector as V

{- | Represents the range of the substring of the input string containing a palindrome.
Format: ([start index (inclusive)], [end index (exclusive)]).
-}
type PalRange = (Int, Int)

{- | Represents a cell in the matrix. Each cell corresponds to some substring of the input
string. The row is the index of the first character (inclusive) and the column is the
index of the last character (inclusive) of the substring.
-}
data Cell = Cell
    { cellColumn :: Int
    , cellBudget :: Budget
    }
    deriving (Show, Eq)

-- | Represents a row in the matrix.
type Row = [Cell]

{- | The budget of a cell is the number of errors that can be added to the substring the
cell corresponds to without exceeding the maximum number of errors.
-}
type Budget = Int

{- | Find all maximal gapped approximate palindromes with a certain gap and a certain
maximum number of errors.
-}
insertionDeletionAlgorithm
    :: (PalEq a)
    => Int
    -- ^ The size of the gap
    -> Int
    -- ^ The maximum number of errors
    -> V.Vector a
    -- ^ The input vector
    -> [PalRange]
    -- ^ The list of found maximal gapped approximate palindromes
insertionDeletionAlgorithm gapSize maxErrors input = concatMap (\(_, y, _) -> y) states
  where
    -- Bound the used gapSize to not be more than the length of the input.
    gapSize' = min gapSize (V.length input)
    -- Use takeIterations to get the states for efficiency.
    states = takeIterations nrOfIterations (fillRow input gapSize' maxErrors) startState
    {- Required number of iterations is (+ 2) to also be able to spot maximal palindromes
    in the two upper rows. -}
    nrOfIterations = maxRow + 2
    startState =
        (
            [ Cell
                { {- We start the matrix on the bottom right, so at the last index of the
                  input. -}
                  cellColumn = lastIndex
                , -- This cell corresponds to an empty substring, so it has no errors.
                  cellBudget = maxErrors
                }
            ] -- This defines the entire bottom row of the matrix.
        , [] -- no maximal palindromes found yet.
        , maxRow - 1 -- row number of the row above the bottom row of the matrix.
        )
    lastIndex = V.length input - 1
    -- The index of the last row, adjusted with the gap size to ignore errors in the gap.
    maxRow = lastIndex - gapSize' + 1

-- | Fills the next row and finds maximal palindromes in the previous row
fillRow
    :: (PalEq a)
    => V.Vector a
    -- ^ Input vector
    -> Int
    -- ^ Maximum size of the gap
    -> Int
    -- ^ Maximum number of errors
    -> (Row, [PalRange], Int)
    -- ^ Old state
    -> (Row, [PalRange], Int)
    -- ^ New state
fillRow input gapSize maxErrors (row, _, rowIndex) =
    (firstCell ++ newRow, foundMaxPals, rowIndex - 1)
  where
    {- The first cell of the current row. A new cell needs to be added at
    the start of this row because we don't add one in the evaluatePosition part
    and every row has one more cell to the left than the previous row. -}
    firstCell
        | initialColumn >= 0 = [Cell{cellColumn = initialColumn, cellBudget = initialBudget}]
        | otherwise = []

    -- The initial column is directly to the left of the diagonal on this row.
    initialColumn = rowIndex + gapSize - 1

    -- The budget of the first cell of this row
    initialBudget :: Budget
    initialBudget
        {- For rows or columns out of bounds, the first cell of the previous row has no
        budget left, because it represents an invalid substring. -}
        | rowIndex < 0 || initialColumn >= V.length input = -1
        {- In general, the first cell of the previous row has a budget of maxErrors,
        because it represent an empty substring, which has no errors. -}
        | otherwise =
            maxErrors

    -- Sparsify the previous row.
    sparsePrevRow =
        sparsify row

    {- Do incremental state generation with output extraction using the previous,
    sparsified row. EvaluatePosition generates tuples with the cell at a position as the
    second element and the cell to the bottomleft of the evaluated position as a found
    gapped approximate maximal palindrome if it is one. The Extractors thus only need to
    extract the second and third elements of the tuples generated by evaluatePostion for
    each position. -}
    (newRow, foundMaxPals) =
        incrementalStateGenerationWithTwoOutputExtractions
            (evaluatePosition input rowIndex)
            (Extractor (\(_, y, _) -> y) (const []))
            (Extractor (\(_, _, z) -> z) (const []))
            {- The row starts with 2 (virtual so they do not appear in the rows) cells
            initialized to maxbudget. One to the left of the current row and one to the
            left of the previous row. -}
            ((maxErrors, maxErrors), [], [])
            sparsePrevRow

{- | Define a new cell (the cell above the input cell) with the correct budget and add
bottom left cell to maxPals if it is maximal.

Consider the following matrix:

@
------------------------------
| topLeft        topRight    |
| bottomLeft     bottomRight |
------------------------------
@

We want to define the budget in topRight and check whether bottomLeft is maximal.
-}
evaluatePosition
    :: (PalEq a)
    => V.Vector a
    -- ^ Input vector
    -> Int
    -- ^ row index
    -> ((Budget, Budget), Row, [PalRange])
    -- ^ Old state
    -> Cell
    -- ^ Input cell
    -> ((Budget, Budget), Row, [PalRange])
    -- ^ New state
evaluatePosition input rowIndex ((topLeft, bottomLeft), _, _) (Cell{cellColumn = column, cellBudget = bottomRight}) =
    ((topRight, bottomRight), [Cell column topRight], maxpals)
  where
    topRight
        | rowIndex >= 0 && column < V.length input =
            maximum
                [ topLeft - 1
                , bottomRight - 1
                , bottomLeft - errorCostAtPosition input (rowIndex, column)
                ]
        | otherwise = -1
    maxpals
        {- add (+ 1) to get inclusive start index and exclusive end index for the found
        maximal palindromes -}
        | bottomLeft >= 0 && topLeft < 0 && topRight < 0 && bottomRight < 0 =
            [(rowIndex + 1, column)]
        | otherwise = []

{- | Replace long sequences of cells with (-1) budgets with two cells with (-1) budgets,
one on either end of the sequence.
-}
sparsify :: Row -> Row
sparsify [] = []
sparsify row@(Cell{cellColumn = firstColumnIndex, cellBudget = _} : _) =
    incrementalStateGenerationWithOutputExtraction
        insertNegatives
        (Extractor snd endf)
        (firstColumnIndex, [])
        (filter ((>= 0) . cellBudget) row)
  where
    insertNegatives :: (Int, Row) -> Cell -> (Int, Row)
    insertNegatives (lastind, _) newCell@Cell{cellColumn = newIndex}
        -- Place two cells on either side of the sequence of negative budgets.
        | newIndex - lastind > 2 =
            ( newIndex
            ,
                [ Cell{cellColumn = lastind + 1, cellBudget = -1}
                , Cell{cellColumn = newIndex - 1, cellBudget = -1}
                , newCell
                ]
            )
        -- Add one cell with (-1) budget back after it has apparently been filtered out before.
        | newIndex - lastind == 2 =
            (newIndex, [Cell{cellColumn = lastind + 1, cellBudget = -1}, newCell])
        -- Do nothing.
        | otherwise = (newIndex, [newCell])

    -- Always add one -1 to the end of the sparsified row
    endf (lastPositiveColumnIndex, _) =
        [Cell{cellColumn = lastPositiveColumnIndex + 1, cellBudget = -1}]

-- | Returns first n elements of iterate f on start.
takeIterations :: Int -> (a -> a) -> a -> [a]
takeIterations n f start = take n $ iterate f start

{- If elements are palindrome equal at position then no error cost, otherwise error cost
of 1 for a substitution error. -}
errorCostAtPosition :: (PalEq a) => V.Vector a -> (Int, Int) -> Int
errorCostAtPosition input (row, column)
    | (input V.! row) =:= (input V.! column) = 0
    | otherwise = 1
