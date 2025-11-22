module MazeV1 where

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
      | currentPos == pos    = renderRoomWith (playerSymbol dir)
      | isDiscoveredPosition = renderRoomWith (tileSymbol tile)
      | otherwise            = renderRoomWith '?'
      where
        currentPos           = (rowIdx, colIdx)
        isDiscoveredPosition = currentPos `elem` disc

    playerSymbol dir = case dir of
      North -> '^'
      East  -> '>'
      South -> 'v'
      West  -> '<'

    tileSymbol tile = case tile of
      Wall  -> 'x'
      Empty -> '_'
      Start -> 's'
      Goal  -> 'o'

    renderRoomWith :: Char -> String
    renderRoomWith c = "[" ++ [c] ++ "]"

--- Dummy (Test Data) ---
testMazeInput :: String
testMazeInput = "[x][x][x][x][x][x][x][x]\n[x][x][x][_][_][x][x][x]\n[s][_][_][_][x][x][o][x]\n[x][x][x][_][x][x][_][x]\n[x][x][x][_][_][_][_][x]\n[x][x][x][x][x][x][x][x]\n"

testMap :: Maze
testMap = parseMap testMazeInput

testGame :: GameState
testGame = newGame testMap
