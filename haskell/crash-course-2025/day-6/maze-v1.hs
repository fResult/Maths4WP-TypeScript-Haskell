module MazeV1 where

data Tile = Wall | Empty | Start | Goal deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq)
type Position = (Int, Int)
type Maze = [[Tile]]

parseMap :: String -> Maze
parseMap input = map parseLine (lines input)
  where
    parseLine [] = []
    parseLine str =
      case break (==']') str of
        ('[':c:_, ']':rest) -> charToTile c : parseLine rest
        _                   -> []

    charToTile c =
      case c of
        'x' -> Wall
        '_' -> Empty
        's' -> Start
        'o' -> Goal
        _   -> Wall

findStart :: Maze -> Position
findStart maze = head [ (rowIdx, colIdx)
                      | (rowIdx, row)  <- zip [0..] grid
                      , (colIdx, tile) <- zip [0..] row
                      , tile == Start ]
                 where grid = maze

--- Dummy (Test Data) ---
testMazeInput :: String
testMazeInput = "[x][x][x][x][x][x][x][x]\n[x][x][x][_][_][x][x][x]\n[s][_][_][_][x][x][o][x]\n[x][x][x][_][x][x][_][x]\n[x][x][x][_][_][_][_][x]\n[x][x][x][x][x][x][x][x]\n"

testMap :: Maze
testMap = parseMap testMazeInput
