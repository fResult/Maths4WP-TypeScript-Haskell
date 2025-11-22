module MazeV1 where

import Data.List (nub)

{-------------------|
|--- Data Models ---|
|-------------------}
data Tile = Wall | Empty | Start | Goal deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq)
type Position = (Int, Int)
type Maze = [MazeRow]
type MazeRow = [Tile]
type Index = Int

data GameState = GameState
  { maze     :: Maze
  , position :: Position
  , direction :: Direction
  , discovered :: [Position]
  } deriving (Show, Eq)

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

-- TODO: Refactor to return `Maybe` or `Either` to handle missing `Start` tile safely.
findStart :: Maze -> Position
findStart maze = head [ (rowIdx, colIdx)
                      | (rowIdx, row)  <- zip [0..] grid
                      , (colIdx, tile) <- zip [0..] row
                      , tile == Start ]
                 where grid = maze

newGame :: Maze -> GameState
newGame maze = GameState maze (findStart maze) East []

{----------------------|
|--- Gameplay Logic ---|
|----------------------}
moveDelta :: Direction -> Position
moveDelta North = (-1, 0)
moveDelta East  = (0, 1)
moveDelta South = (1, 0)
moveDelta West  = (0, -1)

moveForward :: GameState -> (Bool, GameState)
moveForward gs@(GameState maze (rowIdx, colIdx) direction _) =
  let (deltaRowIdx, deltaColIdx) = moveDelta direction
      newPosition                = ( rowIdx + deltaRowIdx
                                   , colIdx + deltaColIdx
                                   ) :: Position
  in case getTile maze newPosition of
    Just tile | isWalkable tile ->
      let newGameState = gs { position = newPosition }
      in (True, reveal newGameState (visibleAround newGameState))
    _ -> (False, gs)

isWalkable :: Tile -> Bool
isWalkable tile = tile `elem` [Empty, Start, Goal]

visibleAround :: GameState -> [Position]
visibleAround (GameState maze (rowIdx, colIdx) direction _) =
  let directions         = [North, East, South, West]
      deltaPositions     = map moveDelta directions
      neighbors          = [(rowIdx + deltaRowIdx, colIdx + deltaColIdx)
                           | (deltaRowIdx, deltaColIdx) <- deltaPositions
                           ]
  in (rowIdx, colIdx) : filter (inBounds maze) neighbors
  where
    inBounds :: Maze -> Position -> Bool
    inBounds maze (rowIdx, colIdx)  = rowIdx >= 0
                                    && colIdx >= 0
                                    && rowIdx < length maze
                                    && colIdx < length cols
      where rows = maze
            cols = head rows

reveal :: GameState -> [Position] -> GameState
reveal gameState positions =
  gameState { discovered = nub (currentDiscovered ++ positions) }
  where
    currentDiscovered = discovered gameState

getTile :: Maze -> Position -> Maybe Tile
getTile maze (rowIdx, colIdx)
  | rowIdx < 0 || colIdx < 0 = Nothing
  | rowIdx >= length rows    = Nothing
  | colIdx >= length columns = Nothing
  | otherwise                = Just (columns `at` colIdx)
  where
    rows        = maze
    columns     = selectedRow
    selectedRow = rows `at` rowIdx
    at          = (!!)

{------------|
|-- Render --|
|------------}
renderMap :: GameState -> String
renderMap gs =
  let tiles = maze gs
      pos   = position gs
      dir   = direction gs
      disc  = discovered gs
  in concatMap (renderRow pos dir disc) (zip [0..] tiles)

  where
    renderRow :: Position -> Direction -> [Position] -> (Index, MazeRow) -> String
    renderRow pos dir disc (rowIdx, row) =
      concatMap (renderCell pos dir disc rowIdx) (zip [0..] row) ++ "\n"

    renderCell :: Position -> Direction -> [Position] -> Index -> (Index, Tile) -> String
    renderCell pos dir disc rowIdx (colIdx, tile)
      | currentPosition == pos = renderRoomWith (playerSymbol dir)
      | isDiscoveredPosition   = renderRoomWith (tileSymbol tile)
      | otherwise              = renderRoomWith '?'
      where
        currentPosition      = (rowIdx, colIdx)
        isDiscoveredPosition = currentPosition `elem` disc

    renderRoomWith :: Char -> String
    renderRoomWith c = "[" ++ [c] ++ "]"

playerSymbol :: Direction -> Char
playerSymbol North = '∧'
playerSymbol East  = '>'
playerSymbol South = '∨'
playerSymbol West  = '<'

-- Why `Start` and `Goal` are `_`?
---- Physically, they're empty walkable tiles, so we render them as Empty ('_')
tileSymbol :: Tile -> Char
tileSymbol Wall  = '#'
tileSymbol Empty = '_'
tileSymbol Start = '_'
tileSymbol Goal  = '_'

{-------------------------|
|--- Dummy (Test Data) ---|
|-------------------------}
testMazeInput :: String
testMazeInput = "[x][x][x][x][x][x][x][x]\n[x][x][x][_][_][x][x][x]\n[s][_][_][_][x][x][o][x]\n[x][x][x][_][x][x][_][x]\n[x][x][x][_][_][_][_][x]\n[x][x][x][x][x][x][x][x]\n"

testMaze :: Maze
testMaze = parseMap testMazeInput

testGame :: GameState
testGame = newGame testMaze
