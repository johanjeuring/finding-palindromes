{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Algorithms.Palindromes.InsertionDeletionAlgorithm where

import Data.Bifunctor (second)

import Data.Algorithms.Palindromes.PalEq (PalEq, (=:=))

import qualified Data.Vector as V

-- | Represents cell location in the matrix. Format is: (row, colunm)
type Position = (Int, Int)

-- | Represents a cell in the matrix. The format is: ((row, column), budget)
data Cell = Cell
    { cellPosition :: Position
    , cellBudget :: Int
    }
    deriving (Show)

-- | Represents a row in the matrix.
type Row = [Cell]

{- Represents 2x2 matrix where:
((a,c),(b,d)) encodes the matrix columnwise
a b
c d
-}
type Matrix2 = ((Int, Int), (Int, Int))

insertionDeletionAlgorithm
    :: (PalEq a)
    => V.Vector a
    -- ^ Input vector
    -> Int
    -- ^ Maximum number of errors
    -> [Position]
    -- ^ Positions of maximal palindromes
insertionDeletionAlgorithm input maxError = concat maxPalindromes
  where
    loopResult =
        foldr
            (insertionDeletionIteration input maxError)
            ([], [Cell (V.length input - 1, V.length input - 1) maxError])
            [0 .. V.length input - 2]
    finalRow = snd loopResult
    maxPalindromesFinalRow = extractMaximalPalindromesFinalRow finalRow
    maxPalindromes = maxPalindromesFinalRow : fst loopResult

insertionDeletionIteration
    :: (PalEq a)
    => V.Vector a
    -- ^ Input vector
    -> Int
    -- ^ Maximum number of errors
    -> Int
    -- ^ Index of current row
    -> ([[Position]], Row)
    -- ^ List of positions of maximal palindromes per row, and the row of the previous iteration.
    -> ([[Position]], Row)
insertionDeletionIteration input maxErrors rowIndex (maxPals, prevRow) = (newMaxPals, newRow)
  where
    denseRow = fillRow input maxErrors rowIndex prevRow
    newMaxPals = extractMaximalPalindromes denseRow : maxPals
    newRow = sparsify (map fst denseRow)

{- Fills a row in the matrix from left to right.
-}
fillRow
    :: (PalEq a)
    => V.Vector a
    -- ^ Input vector
    -> Int
    -- ^ Maximum number of errors
    -> Int
    -- ^ Index of current row
    -> Row
    -- ^ Previous row
    -> [(Cell, Int)]
    -- ^ Current row that has the budget of previous row (the budgets in the cells below)
fillRow input maxErrors rowIndex = scanl getNextBudget (leftMostCell, maxErrors)
  where
    -- The leftmost cell of the row to fill. It always has full budget.
    leftMostCell = Cell (rowIndex, rowIndex) maxErrors
    getNextBudget :: (Cell, Int) -> Cell -> (Cell, Int)
    getNextBudget (Cell prevPosition valLeft, valDiagonal) (Cell _ valBelow) =
        (Cell currentPosition bestBudget, valBelow)
      where
        -- taking left or below budget has error cost of 1 for an insertion
        budgetFromLeft = valLeft - 1
        budgetFromBelow = valBelow - 1
        budgetFromDiagonal = valDiagonal - errorCostAtPosition currentPosition
        -- prevPosition is one column to the left of the current position
        currentPosition = second (+ 1) prevPosition
        bestBudget = maximum [budgetFromLeft, budgetFromBelow, budgetFromDiagonal]

    -- if elements are palindrome equal at position then no error cost, otherwise error cost of 1 for substitution
    errorCostAtPosition :: Position -> Int
    errorCostAtPosition (row, column)
        | (input V.! row) =:= (input V.! column) = 0
        | otherwise = 1

{- Calculates the position of the maximal palindromes in the row below the current row.
It does for the previous since you need the current row to know if the budget is exceeded when the palindrome is extended.
If it is not the palindrome is obviously not maximal.
-}
extractMaximalPalindromes
    :: [(Cell, Int)]
    -- ^ Current row that has the budget of previous row
    -> [Position]
    -- ^ The positions representing maximal palindromes found
extractMaximalPalindromes =
    map fst
        {- Filters positions that do not exceed budget but full triangle to the top right does,
        using the position and budgets from scanr.
        These are maximal since they cannot be extended without exceeding the budget.
        -}
        . filter (\(_, matrix2) -> isMaximal matrix2)
        {- Fills list of same size as the current row with tuple
        (position, (budget above, budget at position), (budget diag, budget right))
        Starts accumulator out of bounds with negative budgets since only able to extend out of bounds makes maximal.
        -}
        . scanr nextMatrix (undefined, ((-1, -1), (-1, -1)))
  where
    nextMatrix
        :: (Cell, Int)
        -- \^ Cell is at the position above the one we want the matrix from, the int is the budget at the position
        -> (Position, Matrix2)
        -> (Position, Matrix2)
    {- Since the scanr is done over current row we add 1 to row since we want position at previous row. -}
    nextMatrix (Cell (row, col) budgetAbove, budget) (_, (fstCol, _)) = ((row + 1, col), ((budgetAbove, budget), fstCol))

{- Matrix:
    a b
    c d

We want to check whether c is the position of a maximal palindrome.
-}
isMaximal :: Matrix2 -> Bool
isMaximal ((a, c), (b, d)) = c >= 0 && a < 0 && b < 0 && d < 0

{- Clears all ranges of negative budgets from a row, except on the borders.
For example row with budgets 0,-1,-1,-1,-1,0 becomes 0,-1,-1,0.
-}
sparsify :: Row -> Row
sparsify row = go [] row Nothing
  where
    go
        :: Row --  Accumulator. Contains the sparsified row thus far in reverse order.
        -> Row -- The rest of the dense row which must still be sparsified.
        -> Maybe Cell -- The last found cell with a non-negative budget.
        -> Row -- The final sparsified row.
        -- row has been completed, return the accumulator, which is now in reverse
    go acc [] _ = reverse acc
    -- while no cell with positive budget is found, start main loop if you find one, else go to next cell
    go acc (cell@(Cell pos budget) : rest) Nothing
        | budget >= 0 = go (cell : acc) rest (Just cell)
        | otherwise = go acc rest Nothing
    -- the main loop, keep filling accumulator and ignoring the strings of cells with negative budgets
    go acc (cell@(Cell (r, c) budget) : rest) (Just prevCell@(Cell (prevR, prevC) _)) =
        case rest of
            [] ->
                if budget < 0
                    then reverse (Cell (prevR, prevC + 1) (-1) : temp)
                    else reverse temp
            _ -> if budget < 0 then go temp rest (Just prevCell) else go temp rest (Just cell)
      where
        temp
            | budget >= 0 =
                case c - prevC of
                    -- just add the cell to accumulator
                    1 -> cell : acc
                    -- Add one cell with a (-1) budget in the gap
                    2 -> cell : Cell (r, c - 1) (-1) : acc
                    -- Add two cells with (-1) budget at the edges of the gap
                    _ -> cell : Cell (r, c - 1) (-1) : Cell (prevR, prevC + 1) (-1) : acc
            | otherwise = acc

{- For the final row, so without a row above it, a cell represents a maximal palindrome
if the cell has a positive budget and the cell to the right of it has a negative budget.
This function uses this property to find the maximal palindromes in the final row.
-}
extractMaximalPalindromesFinalRow :: Row -> [Position]
extractMaximalPalindromesFinalRow row = go row []
  where
    go :: Row -> [Position] -> [Position]
    go [] acc = acc
    go [c] acc
        | cellBudget c >= 0 = cellPosition c : acc
        | otherwise = acc
    go (c0 : c1 : cs) acc
        | cellBudget c0 >= 0 && cellBudget c1 < 0 = go cs (cellPosition c0 : acc)
        | otherwise = go (c1 : cs) acc
