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
type Position = (Int, Int)

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
    -> Int
    -> V.Vector a
    -> [Position]
insertionDeletionAlgorithm gapSize maxErrors input = concatMap get2nd states
  where
    states = iterateTimes nrOfIterations (matIteration input gapSize maxErrors) startState
    -- Required number of iterations is (+ 2) to also be able to spot maximal palindromes in two upper rows
    nrOfIterations = maxRow + 2
    startState =
        ( [Cell (maxRow + gapSize) maxErrors] -- start at the bottom right of the matrix
        , [] -- no maximal palindromes found yet
        , maxRow - 1 -- row number
        )
    -- The index of the last row
    maxRow = V.length input - 1

matIteration
    :: (PalEq a)
    => V.Vector a
    -> Int
    -> Int
    -> (Row, [Position], Int)
    -> (Row, [Position], Int)
matIteration input gapSize maxErrors (row, _, rowIndex) = (newRow, foundMaxPals, rowIndex - 1)
  where
    initialBudget :: Budget
    initialBudget
        | rowIndex < 0 = -1
        | otherwise = maxErrors
    sparsecells = sparsify (Cell (rowIndex + gapSize) initialBudget : row)
    (newRow, foundMaxPals) =
        transducel2
            (evaluatePosition input rowIndex)
            (TransExtract get2nd (const []))
            (TransExtract get3rd (const []))
            ((initialBudget, initialBudget), [], [])
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
    -> Cell
    -- ^ Input cell
    -> ((Budget, Budget), Row, [Position])
    -- ^ Old state for the transducer
    -> ((Budget, Budget), Row, [Position])
    -- ^ New state for the transducer
evaluatePosition input rowIndex (Cell column bottomRight) ((topLeft, bottomLeft), _, _) =
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

    insertNegatives newCell@Cell{cellColumn = newIndex} (lastind, _)
        | newIndex - lastind > 2 =
            (newIndex, [Cell (lastind + 1) (-1), Cell (newIndex - 1) (-1), newCell])
        | newIndex - lastind == 2 = (newIndex, [Cell (lastind + 1) (-1), newCell])
        | otherwise = (newIndex, [newCell])

-- | Returns first n elements of iterate f on start.
iterateTimes :: Int -> (a -> a) -> a -> [a]
iterateTimes n f start = take n $ iterate f start

-- if elements are palindrome equal at position then no error cost, otherwise error cost of 1 for substitution
errorCostAtPosition :: (PalEq a) => V.Vector a -> Position -> Int
errorCostAtPosition input (row, column)
    | (input V.! row) =:= (input V.! column) = 0
    | otherwise = 1

get2nd :: (a, b, c) -> b
get2nd (_, x, _) = x

get3rd :: (a, b, c) -> c
get3rd (_, _, x) = x

-- insertionDeletionAlgorithm
--     :: (PalEq a)
--     => Int
--     -- ^ Maximum number of errors
--     -> V.Vector a
--     -- ^ Input vector
--     -> [Position]
--     -- ^ Positions of maximal palindromes
-- insertionDeletionAlgorithm maxError input =
--     -- Do (+ 1) on the end index to go from inclusive to exclusive
--     map (second (+ 1)) $ concat maxPalindromes
--   where
--     loopResult =
--         foldr
--             (insertionDeletionIteration input maxError)
--             ( []
--             ,
--                 [ Cell
--                     (V.length input - 1, V.length input - 1)
--                     (maxError - errorCostAtPosition input (V.length input - 1, V.length input - 1))
--                 ]
--             )
--             [0 .. V.length input - 2]
--     finalRow = snd loopResult
--     maxPalindromesFinalRow = extractMaximalPalindromesFinalRow finalRow
--     maxPalindromes = maxPalindromesFinalRow : fst loopResult

-- insertionDeletionIteration
--     :: (PalEq a)
--     => V.Vector a
--     -- ^ Input vector
--     -> Int
--     -- ^ Maximum number of errors
--     -> Int
--     -- ^ Index of current row
--     -> ([[Position]], Row)
--     -- ^ List of positions of maximal palindromes per row, and the row of the previous iteration
--     -> ([[Position]], Row)
-- insertionDeletionIteration input maxErrors rowIndex (maxPals, prevRow) = (newMaxPals, newRow)
--   where
--     denseRow = fillRow input maxErrors rowIndex prevRow
--     newMaxPals = extractMaximalPalindromes denseRow : maxPals
--     newRow = sparsify (length input) (map fst denseRow)

-- -- | Fills a row in the matrix from left to right.
-- fillRow
--     :: (PalEq a)
--     => V.Vector a
--     -- ^ Input vector
--     -> Int
--     -- ^ Maximum number of errors
--     -> Int
--     -- ^ Index of current row
--     -> Row
--     -- ^ Previous row
--     -> [(Cell, Int)]
--     -- ^ Current row that has the budget of previous row (the budgets in the cells below)
-- fillRow input maxErrors rowIndex = scanl getNextBudget (leftMostCell, maxErrors)
--   where
--     {- The leftmost cell of the row to fill. It has full budget.
--     The error cost at position is removed because not every datatype has garantueed single character palindromes. -}
--     leftMostCell = Cell (rowIndex, rowIndex) (maxErrors - errorCostAtPosition input (rowIndex, rowIndex))
--     {- Given previous cell, the budget of the cell below that and the cell below,
--     calculate the budget for a new cell. -}
--     getNextBudget :: (Cell, Int) -> Cell -> (Cell, Int)
--     getNextBudget (Cell prevPosition valLeft, valDiagonal) (Cell _ valBelow) =
--         (Cell currentPosition bestBudget, valBelow)
--       where
--         -- taking left or below budget has error cost of 1 for an insertion
--         budgetFromLeft = valLeft - 1
--         budgetFromBelow = valBelow - 1
--         {- taking diagonal budget has error cost of 1 or 0, depending on whether the
--         elements match at the current position -}
--         budgetFromDiagonal = valDiagonal - errorCostAtPosition input currentPosition
--         -- prevPosition is one column to the left of the current position
--         currentPosition = second (+ 1) prevPosition
--         bestBudget = maximum [budgetFromLeft, budgetFromBelow, budgetFromDiagonal]

-- {- Calculates the position of the maximal palindromes in the row below the current row.
-- It does for the previous since you need the current row to know if the budget is exceeded when the palindrome is extended.
-- If it is not the palindrome is obviously not maximal.
-- -}
-- extractMaximalPalindromes
--     :: [(Cell, Int)]
--     -- ^ Current row that has the budget of previous row
--     -> [Position]
--     -- ^ The positions representing maximal palindromes found
-- extractMaximalPalindromes =
--     map fst
--         {- Filters positions that do not exceed budget but full triangle to the top right
--         does, using the position and budgets from scanr. These are maximal since they
--         cannot be extended without exceeding the budget.
--         -}
--         . filter (\(_, matrix2) -> isMaximal matrix2)
--         {- Fills list of same size as the current row with tuple (position, (budget above,
--         budget at position), (budget diag, budget right)). Starts accumulator out of
--         bounds with negative budgets since only able to extend out of bounds makes a
--         position maximal.
--         -}
--         . scanr nextMatrix (undefined, ((-1, -1), (-1, -1)))
--   where
--     -- Gets the 2x2 matrix one position to the left of the current position.
--     nextMatrix
--         :: (Cell, Int)
--         -- The cell is at the position above the one we want the matrix for, the int is the
--         --        budget at the position.
--         -> (Position, Matrix2)
--         -> (Position, Matrix2)
--     {- Since the scanr is done over current row we add 1 to row since we want position at
--     previous row. -}
--     nextMatrix (Cell (row, col) budgetAbove, budget) (_, (fstCol, _)) =
--         ((row + 1, col), ((budgetAbove, budget), fstCol))

-- {- Matrix:
-- a b
-- c d

-- We want to check whether c is the position of a maximal palindrome. This is only the case
-- if c is non-negative and the rest of the matrix is negative.
-- -}
-- isMaximal :: Matrix2 -> Bool
-- isMaximal ((a, c), (b, d)) = c >= 0 && a < 0 && b < 0 && d < 0

-- {- For the final row, so without a row above it, a cell represents a maximal palindrome
-- if the cell has a positive budget and the cell to the right of it has a negative budget.
-- This function uses this property to find the maximal palindromes in the final row.
-- -}
-- extractMaximalPalindromesFinalRow :: Row -> [Position]
-- extractMaximalPalindromesFinalRow row = go row []
--   where
--     go :: Row -> [Position] -> [Position]
--     go [] acc = acc
--     -- The final cell is maximal only if budget is non-negative.
--     go [c] acc
--         | cellBudget c >= 0 = cellPosition c : acc
--         | otherwise = acc
--     go (c0 : c1 : cs) acc
--         | cellBudget c0 >= 0 && cellBudget c1 < 0 = go cs (cellPosition c0 : acc)
--         | otherwise = go (c1 : cs) acc

-- {- | For (large) substrings of cell with negative budgets, put one cell with (-1) budget
-- at each end of it. This saves unnecessary memory use.
-- -}
-- sparsify :: Int -> Row -> Row
-- sparsify _ [] = []
-- sparsify inputLength row@(firstCell : tail) = reverse (extraCell ++ sparsifiedReversed)
--   where
--     -- remove negative budgets from the tail
--     filteredRow = firstCell : filter ((>= 0) . cellBudget) tail

--     -- At the start and end of removed sequence of negatives in the filtered row inserts negative cells.
--     (lastCell, sparsifiedReversed) = foldl insertNegatives (Nothing, []) filteredRow

--     {- Before a cell is added to the sparsifyReversed list inserts a negative cell on both sides of a gap
--     when there is one. Based on the position of last positive budget cell and the current cell.
--     -}
--     insertNegatives :: (Maybe Cell, Row) -> Cell -> (Maybe Cell, Row)
--     insertNegatives (Nothing, _) cell = (Just cell, [cell])
--     insertNegatives (Just (Cell (_, prevC) _), acc) newCell@(Cell (r, c) _)
--         | c - prevC > 2 =
--             (Just newCell, newCell : Cell (r, c - 1) (-1) : Cell (r, prevC + 1) (-1) : acc)
--         | c - prevC == 2 = (Just newCell, newCell : Cell (r, prevC + 1) (-1) : acc)
--         | otherwise = (Just newCell, newCell : acc)

--     -- We need a negative cell at the end of the row, unless this is out of bounds of the input.
--     extraCell = case lastCell of
--         Nothing -> []
--         Just (Cell (lastR, lastC) _) -> ([Cell (lastR, lastC + 1) (-1) | lastC < inputLength - 1])

-- -- | Returns first n elements of iterate f on start.
-- iterateTimes :: Int -> (a -> a) -> a -> [a]
-- iterateTimes n f start = take n $ iterate f start
