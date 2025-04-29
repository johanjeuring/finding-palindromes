module UTLinearAlgorithm (testListInsertionDeletionAlgorithm) where

import InsertionDeletionAlgorithm (Cell, sparsify)
import Test.HUnit (Test (..), assertEqual)

testListInsertionDeletionAlgorithm = [testSparsifySimple, testSparsifyEdgeCase, testSparsifyComplex]

testSparsifySimple :: Test
testSparsifySimple =
    TestCase $
        assertEqual
            "testSparsifySimple"
            ( sparsify
                [ Cell (0, 0) 1
                , Cell (0, 1) 0
                , Cell (0, 2) (-1)
                , Cell (0, 3) (-1)
                , Cell (0, 4) (-1)
                , Cell (0, 5) (-1)
                , Cell (0, 6) 0
                ]
            )
            [Cell (0, 0) 1, Cell (0, 1) 0, Cell (0, 2) (-1), Cell (0, 5) (-1), Cell (0, 6) 0]

testSparsifyEdgeCase :: Test
testSparsifyEdgeCase =
    TestCase $
        assertEqual
            "testSparsifyEdgeCase"
            (sparsify [1, 0, -1, 0])
            [1, 0, -1, 0]

testSparsifyComplex :: Test
testSparsifyComplex =
    TestCase $
        assertEqual
            "testSparsifySimple"
            (sparsify [1, 0, -1, -1, -1, -1, 0, 1, 0, -1, 0, 0, 0, -1, -2, -3, -4])
            [1, 0, -1, -1, 0, 1, 0, -1, 0, 0, 0, -1, -1]
