{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Algorithms.Palindromes.InsertionDeletionAlgorithm where

import Data.Algorithms.Palindromes.PalEq (PalEq, (=:=))
import Data.Algorithms.Palindromes.Transducers
    ( TransduceExtractor (..)
    , transducel1
    , transducel2
    )

import qualified Data.Vector as V

-- | Represents cell location in the matrix. Format is: (row, colunm)
type PalRange = (Int, Int)

-- | Represents a cell in the matrix. The format is: (column, budget)
data Cell = Cell
    { cellColumn :: Int
    , cellBudget :: Budget
    }
    deriving (Show, Eq)

-- | Represents a row in the matrix.
type Row = [Cell]

type Budget = Int

insertionDeletionAlgorithm
    :: (PalEq a)
    => Int
    -- ^ The maximum size of the gap
    -> Int
    -- ^ The maximum number of errors
    -> V.Vector a
    -- ^ The input vector
    -> [PalRange]
--
insertionDeletionAlgorithm gapSize maxErrors input = concatMap (\(_, y, _) -> y) states
  where
    states = iterateTimes nrOfIterations (fillRow input gapSize maxErrors) startState
    -- Required number of iterations is (+ 2) to also be able to spot maximal palindromes in two upper rows
    nrOfIterations = maxRow + 2
    startState =
        (
            [ Cell
                startColumn
                startBudget
            ] -- start at the bottom right of the matrix
        , [] -- no maximal palindromes found yet
        , maxRow - 1 -- row number
        )
    -- The index of the last row
    maxRow = V.length input - 1
    -- The rightmost column we use
    startColumn = maxRow + gapSize
    -- The budget for the rightmost cell of the matrix
    startBudget
        | gapSize > 0 = -1
        | otherwise = maxErrors - errorCostAtPosition input (maxRow, startColumn)

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
    -- ^ Old state of the transducer
    -> (Row, [PalRange], Int)
    -- ^ New state of the transducer
fillRow input gapSize maxErrors (row, _, rowIndex) = (newRow, foundMaxPals, rowIndex - 1)
  where
    sparsecells = sparsify (Cell initialColumn initialBudget : row)
    {- start with the leftmost cell in the row, which is shifted by gapSize.
       A new cell needs to be added at the start of the row because we don't add one in the evaluatepositon part.
       every row is extended one to the left when you go up by one (along the diagonal)-}
    initialColumn = rowIndex + gapSize
    initialBudget :: Budget
    initialBudget
        | rowIndex == -1 && initialColumn >= 0 = maxErrors
        | rowIndex < 0 = -1
        | initialColumn >= V.length input = -1
        | otherwise =
            maxErrors
    (newRow, foundMaxPals) =
        transducel2
            (evaluatePosition input rowIndex)
            (TransExtract (\(_, y, _) -> y) (const []))
            (TransExtract (\(_, _, z) -> z) (const []))
            ((maxErrors, maxErrors), [], [])
            {- the row starts with 2 (virtual so they do not appear in the rows) cells initialized to maxbudget
            one as initializer for current row and one for the previous row -}
            sparsecells

{- | Define a new cell (the cell above the input cell) with the correct budget and add
bottom left cell to maxPals if it is maximal.

Consider the following matrix:
------------------------------
| topLeft        topRight    |
| bottomLeft     bottomRight |
------------------------------

We want to define the budget in topRight and check whether bottomLeft is maximal.
-}
evaluatePosition
    :: (PalEq a)
    => V.Vector a
    -- ^ Input vector
    -> Int
    -- ^ row index
    -> ((Budget, Budget), Row, [PalRange])
    -- ^ Old state for the transducer
    -> Cell
    -- ^ Input cell
    -> ((Budget, Budget), Row, [PalRange])
    -- ^ New state for the transducer
evaluatePosition input rowIndex ((topLeft, bottomLeft), _, _) (Cell column bottomRight) =
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
    maxpals =
        {- add (+ 1) to get inclusive start index and exclusive end index for the found
        maximal palindromes -}
        [ (rowIndex + 1, column)
        | bottomLeft >= 0 && topLeft < 0 && topRight < 0 && bottomRight < 0
        ]

sparsify :: Row -> Row
sparsify [] = []
sparsify row@(Cell firstColumnIndex _ : _) =
    transducel1
        insertNegatives
        (TransExtract snd endf)
        (firstColumnIndex, [])
        (filter ((>= 0) . cellBudget) row)
  where
    -- Always add two (-1)'s to the end of the sparsified row
    endf (lastPositiveColumnIndex, _) =
        [Cell (lastPositiveColumnIndex + 1) (-1), Cell (lastPositiveColumnIndex + 2) (-1)]

    insertNegatives :: (Int, Row) -> Cell -> (Int, Row)
    insertNegatives (lastind, _) newCell@Cell{cellColumn = newIndex}
        | newIndex - lastind > 2 =
            (newIndex, [Cell (lastind + 1) (-1), Cell (newIndex - 1) (-1), newCell])
        | newIndex - lastind == 2 = (newIndex, [Cell (lastind + 1) (-1), newCell])
        | otherwise = (newIndex, [newCell])

-- | Returns first n elements of iterate f on start.
iterateTimes :: Int -> (a -> a) -> a -> [a]
iterateTimes n f start = take n $ iterate f start

-- if elements are palindrome equal at position then no error cost, otherwise error cost of 1 for substitution
errorCostAtPosition :: (PalEq a) => V.Vector a -> (Int, Int) -> Int
errorCostAtPosition input (row, column)
    | (input V.! row) =:= (input V.! column) = 0
    | otherwise = 1
