module MazeV2 where

import Data.List (nub)
import Data.Char (toUpper, toLower)
import System.IO (hFlush, stdout)

import ParserV1

{--------------------------|
|--- Functional Parsers ---|
|--------------------------}
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

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
  } deriving (Show)

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
newGame maze = reveal initialState (visibleAround initialState)
  where
    startPosition = findStart maze
    initialState  = GameState maze startPosition East []

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

arrivedGoal :: GameState -> Bool
arrivedGoal (GameState maze position _ _) = tile == Just Goal
  where
    tile = getTile maze position

turnLeft :: Direction -> Direction
turnLeft direction = case direction of
  North -> West
  West  -> South
  South -> East
  East  -> North

turnRight :: Direction -> Direction
turnRight direction = case direction of
  North -> East
  East  -> South
  South -> West
  West  -> North

inBounds :: Maze -> Position -> Bool
inBounds maze (rowIdx, colIdx) = rowIdxValid && colIdxValid
  where
    rowIdxValid = rowIdx >= 0 && rowIdx < length rows
    colIdxValid = colIdx >= 0 && colIdx < length cols
    rows        = maze
    cols        = selectedRow
    selectedRow = rows `at` rowIdx
    at          = (!!)

lookAround :: GameState -> (String, GameState)
lookAround gameState = (desc, revealed)
  where
    desc = describeSurrounding gameState
    revealed = reveal gameState (visibleAround gameState)

describeSurrounding :: GameState -> String
describeSurrounding (GameState maze (rowIdx, colIdx) direction _) = unwords
  [ "You see", front, "in front of you."
  , capitalize left, "to the left."
  , capitalize right, "to the right."
  ]
  where
    front        = lookDirectTo direction
    left         = lookDirectTo (turnLeft direction)
    right        = lookDirectTo (turnRight direction)
    lookDirectTo direction = describe (getTile maze (rowIdx + deltaRowIdx, colIdx + deltaColIdx))
      where
        (deltaRowIdx, deltaColIdx) = moveDelta direction

    describe :: Maybe Tile -> String
    describe (Just Wall)  = "a wall"
    describe (Just Empty) = "a path"
    describe (Just Start) = "a path"
    describe (Just Goal)  = "a goal"
    describe Nothing      = "a void"


visibleAround :: GameState -> [Position]
visibleAround (GameState maze (rowIdx, colIdx) direction _) =
  let directions         = [North, East, South, West]
      deltaPositions     = map moveDelta directions
      neighbors          = [(rowIdx + deltaRowIdx, colIdx + deltaColIdx)
                           | (deltaRowIdx, deltaColIdx) <- deltaPositions
                           ]
  in (rowIdx, colIdx) : filter (inBounds maze) neighbors

reveal :: GameState -> [Position] -> GameState
reveal gameState positions =
  gameState { discovered = nub (currentDiscovered ++ positions) }
  where
    currentDiscovered = discovered gameState

getTile :: Maze -> Position -> Maybe Tile
getTile maze position
  | inBounds maze position = Just (columns `at` colIdx)
  | otherwise              = Nothing
  where
    columns = maze `at` fst position
    rowIdx  = fst position
    colIdx  = snd position
    at      = (!!)

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

{---------------|
|-- Game Loop --|
|---------------}
gameLoop :: GameState -> IO ()
gameLoop gameState = do
  putStr "\ESC[31m➜ \ESC[34m"
  hFlush stdout
  cmd <- fmap (map toLower) getLine
  putStr "\ESC[m"
  case cmd of
    "forward"    -> handleMoveForward gameState
    "turn left"  -> handleTurnLeft gameState
    "turn right" -> handleTurnRight gameState
    "look"       -> handleLook gameState
    "map"        -> handleMap gameState
    "quit"       -> handleQuit
    _            -> handleUnknown gameState

handleMoveForward :: GameState -> IO ()
handleMoveForward gameState = if isWalkable
  then do
    putStrLn "You moved forward."
    afterAction newGameState
  else do
    putStrLn "You hit the wall."
    gameLoop gameState
  where
    (isWalkable, newGameState) = moveForward gameState

handleTurnLeft :: GameState -> IO ()
handleTurnLeft gameState = do
  let turned   = gameState { direction = turnLeft (direction gameState) }
      revealed = reveal turned (visibleAround turned)
  putStrLn "You turn left."
  afterAction revealed

handleTurnRight :: GameState -> IO ()
handleTurnRight gameState = do
  let turned   = gameState { direction = turnRight (direction gameState) }
      revealed = reveal turned (visibleAround turned)
  putStrLn "You turn right."
  gameLoop turned

afterAction :: GameState -> IO ()
afterAction gameState = do
  let (desc, revealed) = lookAround gameState
  putStrLn desc
  if arrivedGoal revealed
    then putStrLn "🎉 You reached the goal! 🎉"
    else gameLoop revealed

handleLook :: GameState -> IO ()
handleLook gameState = do
  let (description, revealed) = lookAround gameState
  putStrLn description
  gameLoop revealed

handleMap :: GameState -> IO ()
handleMap gameState = do
  putStrLn $ renderMap gameState
  gameLoop gameState


handleQuit :: IO ()
handleQuit = putStrLn "Goodbye!"

handleUnknown :: GameState -> IO ()
handleUnknown gameState = do
  putStrLn "Unknown command. Try: forward | turn left | turn right | look | map | help | quit"
  gameLoop gameState

{----------|
|-- Main --|
|----------}
main :: IO ()
main = do
  test

  putStrLn ""
  putStrLn "-----------------------------"
  putStrLn "--------- Start app ---------"
  putStrLn "-----------------------------"
  -- haskell/crash-course-2025/maze-maps/maze1.txt
  mazeGrid <- readFile "../maze-maps/maze1.txt"

  let game = newGame $ parseMap mazeGrid
  putStrLn "Welcome to the maze!"
  putStrLn "Commands: forward | turn left | turn right | look | map | help | quit"
  gameLoop game

{-----------|
|-- Misc. --|
|-----------}
capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : map toLower xs

{-------------------------|
|--- Dummy (Test Data) ---|
|-------------------------}
testMazeInput :: String
testMazeInput = "[x][x][x][x][x][x][x][x]\n[x][x][x][_][_][x][x][x]\n[s][_][_][_][x][x][o][x]\n[x][x][x][_][x][x][_][x]\n[x][x][x][_][_][_][_][x]\n[x][x][x][x][x][x][x][x]\n"

testMaze :: Maze
testMaze = parseMap testMazeInput

testGame :: GameState
testGame = newGame testMaze

{--------------------|
|--- Unit Testing ---|
|--------------------}
test :: IO ()
test = do
  let gameAtStart = testGame
  let gameAtGoal = gameAtStart { position = (2, 6) }

  putStrLn "------- Testing Direction Helpers -------"
  do
    let expectedLeft = West
    let actualLeft   = turnLeft North
    putStrLn $ "\tNorth turnLeft -> " ++ show actualLeft
    assertAndLog expectedLeft actualLeft

    let expectedRight = East
    let actualRight   = turnRight North
    putStrLn $ "\tNorth turnRight -> " ++ show actualRight
    assertAndLog expectedRight actualRight

  putStrLn "\n------- Testing Goal Check -------"
  do
    let expectedArrivedGoalAtStart = False
    let actualArrivedGoalAtStart = arrivedGoal gameAtStart
    putStrLn $ "\tAt Start is Goal? -> " ++ show actualArrivedGoalAtStart
    assertAndLog expectedArrivedGoalAtStart actualArrivedGoalAtStart

    let expectedArrivedGoalAtGoal = True
    let actualArrivedGoalAtGoal = arrivedGoal gameAtGoal
    putStrLn $ "\tAt (2,6) is Goal? -> " ++ show actualArrivedGoalAtGoal
    assertAndLog expectedArrivedGoalAtGoal actualArrivedGoalAtGoal

  putStrLn "\n------- Testing Movement & Fog of War -------"
  do
    let (actualMoved, actualNextState) = moveForward gameAtStart

    -- Check 1: Movement Success
    let expectedMoved = True
    putStrLn $ "\tMove Success? -> " ++ show actualMoved
    assertAndLog expectedMoved actualMoved

    -- Check 2: New Position
    let expectedPosition = (2, 1)
    let actualPosition = position actualNextState
    putStrLn $ "\tNew Position -> " ++ show actualPosition
    assertAndLog expectedPosition actualPosition

    -- Check 3: Fog of War (Discovered Count)
    let expectedDiscoveredCount = 5
    let actualDiscoveredCount = length (discovered actualNextState)
    putStrLn $ "\tDiscovered Count -> " ++ show actualDiscoveredCount
    assertAndLog expectedDiscoveredCount actualDiscoveredCount

  putStrLn "\n------- Testing Look Surrounding -------"
  do
    let (actualDesc, _) = lookAround gameAtStart
    let expectedDesc = "You see a path in front of you. A wall to the left. A wall to the right."
    putStrLn $ "\tDescription -> " ++ show actualDesc
    assertAndLog expectedDesc actualDesc

assertAndLog :: (Show a, Eq a) => a -> a -> IO ()
assertAndLog expected actual =
  putStrLn $ "\t[Check] Expected \""
  ++ show expected
  ++ "\" -> "
  ++ show (expected == actual)
