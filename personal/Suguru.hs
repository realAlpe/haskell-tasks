module Suguru where

import Data.Map qualified as Map

data Cell = Cell {row, col, group, value :: Int}
  deriving (Show, Eq, Ord)

type Board = [[Cell]]

data Group = Group {groupId :: Int, groupMembers :: [Cell]}

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

parseBoard :: String -> Board
parseBoard = map parseRow . enumerate . lines
  where
    parseRow :: (Int, String) -> [Cell]
    parseRow (i, row) = map (parseCell i) (enumerate $ words row)
    parseCell :: Int -> (Int, String) -> Cell
    parseCell i (j, cell) = Cell {row = i, col = j, group = read cell, value = 0}

inputBoard :: String
inputBoard =
  unlines
    [ "2 3 3 1 1 1",
      "2 4 4 4 1 1",
      "5 5 4 4 4 1",
      "5 2 6 6 6 1",
      "5 7 7 6 1 8",
      "7 7 7 7 8 8"
    ]

countGroups :: [Cell] -> Map.Map Int Int
countGroups = foldl (\acc x -> Map.insertWith (+) (group x) 1 acc) Map.empty

isValidBoard :: Board -> Bool
isValidBoard board = all validCell boardCells
  where
    boardCells = concat board
    groupIdToSize = countGroups boardCells
    validCell :: Cell -> Bool
    validCell cell = validValue cell && validAdjacentCells cell
    validValue :: Cell -> Bool
    validValue cell =
      1 <= value cell && case Map.lookup (group cell) groupIdToSize of
        Just groupSize -> value cell <= groupSize
        Nothing -> True
    validAdjacentCells :: Cell -> Bool
    validAdjacentCells cell = not $ any (validAdjacentValues cell) boardCells

validAdjacentValues :: Cell -> Cell -> Bool
validAdjacentValues c1 c2 =
  (v1 == v2 && v2 == 0)
    || v1 /= v2
    || abs (row c1 - row c2) /= 1
    || abs (col c1 - col c2) /= 1
  where
    v1 = value c1
    v2 = value c2

-- also have to add check in same group...
b :: Board
b = parseBoard inputBoard

cc1 = Cell 2 3 1 3

cc2 = Cell 4 2 2 3
