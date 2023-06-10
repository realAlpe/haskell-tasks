module Sudoku where

import Data.List (elemIndex, transpose)
import Data.Set (Set, fromList)

-- Size is 9x9
type Board = [[Int]]

isValid :: Board -> Int -> (Int, Int) -> Bool
isValid board num (row, col) =
  let rowVals = board !! row
      colVals = map (!! col) board
      boxRow = (row `div` 3) * 3
      boxCol = (col `div` 3) * 3
      boxVals =
        [ board !! r !! c
          | r <- [boxRow .. boxRow + 2],
            c <- [boxCol .. boxCol + 2]
        ]
   in num `notElem` rowVals
        && num `notElem` colVals
        && num `notElem` boxVals

getRow :: Board -> Int -> Set Int
getRow board row = fromList (board !! row)

getCol :: Board -> Int -> Set Int
getCol board = getRow (transpose board)

getBox :: Board -> Int -> Int -> Set Int
getBox board row col =
  fromList
    [ board !! r !! c
      | r <- [boxRow .. boxRow + 2],
        c <- [boxCol .. boxCol + 2]
    ]
  where
    boxRow = (row `div` 3) * 3
    boxCol = (col `div` 3) * 3

findEmpty :: Board -> Maybe (Int, Int)
findEmpty board = tmp $ elemIndex 0 (concat board)
  where
    tmp Nothing = Nothing
    tmp (Just index) = Just (divMod index 9)

solveSudoku :: Board -> Maybe Board
solveSudoku board =
  case findEmpty board of
    Nothing -> Just board
    Just (row, col) -> Nothing

insa :: Board -> Int -> Int -> Int -> Board
insa board row col num =
  undefined