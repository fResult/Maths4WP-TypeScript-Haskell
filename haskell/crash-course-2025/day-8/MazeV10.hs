module MazeV10 where

import Control.Applicative ((<|>), Alternative (some, many))
import Data.Char (toUpper, toLower, isSpace, isAlphaNum)
import Data.Functor (($>))
import Data.List (nub)
import System.Environment (getArgs, getProgName)
import System.IO (hFlush, stdout)

import ParserV5 ( word
                , Parser(runParser)
                , satisfy
                , parseByLines
                , runParserUnsafe
                , parseInt
                , between
                )
import Control.Monad.State ( MonadState(get, put)
                           , StateT
                           , MonadIO (liftIO)
                           , gets
                           , evalStateT, modify
                           )

data Color = Color
  { info    :: String
  , success :: String
  , warning :: String
  , danger  :: String
  , reset   :: String
  }
colors :: Color
colors = Color
  { info    = "\ESC[36m"
  , success = "\ESC[32m"
  , warning = "\ESC[33m"
  , danger  = "\ESC[31m"
  , reset   = "\ESC[m"
  }

{-------------------|
|--- Data Models ---|
|-------------------}
data Tile = Wall | Empty | Start | Goal deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq)
type Position = (Int, Int)
type MazeLayout = [MazeRow]
type MazeRow = [Tile]
type Index = Int
type ErrorMessage = String

type Alias = String
type AliasEnv = [(Alias, Action)]

newtype Maze = Maze
  { layout :: MazeLayout
  -- Future extension (Require changing `newtype` to `data`):
  -- - difficultyScore: Layout complexity score
  -- - shortestRoute: Precomputed optimal path
  -- - bestActionSeq: Ideal Sequence of the action
  -- - maxTreasures: Total spawnable items
  --
  -- Boundary Note:
  -- Game rules (e.g., Easy = 50x shortest route steps, ..., Nightmare = strict steps) belong in `GameState`, not here
  } deriving (Show)

data GameState = GameState
  { maze     :: Maze
  , position :: Position
  , direction :: Direction
  , discovered :: [Position]
  , aliases :: AliasEnv
  } deriving (Show)

data Action
  = Forward
  | TurnLeft
  | TurnRight
  | Turn Direction
  | Look
  | Map
  | Quit
  | Help
  | Sequence [Action]
  | Repeat Int Action
  | Assign Alias Action
  | Use Alias
  | Unknown String
  deriving (Show, Eq)

{-------------------------|
|--- Game State Monads ---|
|-------------------------}
type Game a = StateT GameState IO a

{--------------------|
|--- Game Parsers ---|
|--------------------}
-- parseInput :: String -> Maybe Action
-- parseInput input = case run input of
--   Just (a, _) -> Just a
--   Nothing     -> Nothing
--   where
--     run = runParser parseSequence

-- parseAction :: Parser Action
-- parseAction =
--       parseAssign
--   <|> parseUse
--   <|> parseRepeatPrefix
--   <|> parseRepeatPostfix
--   <|> parseAtomicAction
--   <|> parseUnknown

-- parseSequence :: Parser Action
-- parseSequence = do
--   first <- parseAction
--   rest  <- many (word "then" *> parseAction)
--   pure $
--     case rest of
--       []  -> first
--       _   -> Sequence (first : rest)

-- parseRepeatPostfix :: Parser Action
-- parseRepeatPostfix = do
--   action <- parseParenthesesOrAction
--   n <- parseInt
--   pure (Repeat n action)


-- parseRepeatPrefix :: Parser Action
-- parseRepeatPrefix = do
--   word "repeat"
--   n <- parseInt
--   action <- parseParenthesesOrAction
--   pure (Repeat n action)

-- parseAtomicAction :: Parser Action
-- parseAtomicAction =
--       word "forward" $> Forward
--   <|> word "move" *> word "forward" $> Forward
--   <|> word "turn" *> word "left" $> TurnLeft
--   <|> word "turn" *> word "right" $> TurnRight
--   <|> word "left" $> TurnLeft
--   <|> word "right" $> TurnRight
--   <|> word "turn" *> (Turn <$> parseDirection)
--   <|> Turn <$> parseDirection
--   <|> word "look" $> Look
--   <|> word "map" $> Map
--   <|> word "help" $> Help
--   <|> word "command" $> Help
--   <|> word "quit" $> Quit
--   <|> parseUnknown

-- parseParenthesesOrAction :: Parser Action
-- parseParenthesesOrAction =
--       between (word "(") (word ")") parseSequence
--   <|> parseAtomicAction

-- parseUse :: Parser Action
-- parseUse = do
--   word "use"
--   name <- some (satisfy isAlphaNum)
--   pure (Use name)

-- parseMap :: Parser Maze
-- parseMap = Maze <$> parseByLines parseRow

-- parseRow :: Parser MazeRow -- Parser [Tile]
-- parseRow = some parseTile

-- parseTile :: Parser Tile
-- parseTile =
--       word "[x]" $> Wall
--   <|> word "[_]" $> Empty
--   <|> word "[s]" $> Start
--   <|> word "[o]" $> Goal

-- parseUnknown ::  Parser Action
-- parseUnknown = do
--   command <- some (satisfy (\c -> not (isSpace c)))
--   pure $ Unknown command

-- Top level: either assignment or expression
parseAction :: Parser Action
parseAction =
      parseAssign
  <|> parseExpression

-- name ::= expression
parseAssign :: Parser Action
parseAssign = do
  name <- some (satisfy isAlphaNum)
  word "="
  body <- parseExpression
  pure $ Assign name body

-- expression ::= term ("then" term)*
parseExpression :: Parser Action
parseExpression = do
  first <- parseTerm
  rest  <- many (word "then" *> parseTerm)
  pure $
    case rest of
      [] -> first
      xs -> Sequence (first : xs)

-- term ::= "repeat" n atom/parentheses
--        | atom/parentheses n?
parseTerm :: Parser Action
parseTerm = undefined

--

parseDirection :: Parser Direction
parseDirection =
      word "north" $> North
  <|> word "east"  $> East
  <|> word "south" $> South
  <|> word "west"  $> West

--

-- TODO: Refactor to return `Maybe` or `Either` to handle missing `Start` tile safely.
findStart :: Maze -> Position
findStart maze = head [ (rowIdx, colIdx)
                      | (rowIdx, row)  <- zip [0..] $ grid
                      , (colIdx, tile) <- zip [0..] row
                      , tile == Start ]
                 where grid = layout maze

newGame :: Maze -> GameState
newGame maze = updateVision initialState
  where
    startPosition = findStart maze
    initialState  = GameState maze startPosition East [] []

{----------------------|
|--- Gameplay Logic ---|
|----------------------}
moveDelta :: Direction -> Position
moveDelta North = (-1, 0)
moveDelta East  = (0, 1)
moveDelta South = (1, 0)
moveDelta West  = (0, -1)

moveForward :: GameState -> (Bool, GameState)
moveForward gs@(GameState maze (rowIdx, colIdx) direction _ _) =
  let (deltaRowIdx, deltaColIdx) = moveDelta direction
      newPosition                = ( rowIdx + deltaRowIdx
                                   , colIdx + deltaColIdx
                                   ) :: Position
  in case getTile maze newPosition of
    Just tile | isWalkable tile ->
      let newGameState = gs { position = newPosition }
      in (True, updateVision newGameState)
    _ -> (False, gs)

isWalkable :: Tile -> Bool
isWalkable tile = tile `elem` [Empty, Start, Goal]

arrivedGoal :: GameState -> Bool
arrivedGoal (GameState maze position _ _ _) = tile == Just Goal
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

inBounds :: MazeLayout -> Position -> Bool
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
    desc = info colors ++ (describeSurrounding gameState) ++ reset colors
    revealed = updateVision gameState

describeSurrounding :: GameState -> String
describeSurrounding (GameState maze (rowIdx, colIdx) direction _ _) = unwords
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

updateVision :: GameState -> GameState
updateVision gs = reveal gs (visibleAround gs)

visibleAround :: GameState -> [Position]
visibleAround (GameState maze (rowIdx, colIdx) direction _ _) =
  let directions         = [North, East, South, West]
      deltaPositions     = map moveDelta directions
      neighbors          = [(rowIdx + deltaRowIdx, colIdx + deltaColIdx)
                           | (deltaRowIdx, deltaColIdx) <- deltaPositions
                           ]
  in (rowIdx, colIdx) : filter (inBounds (layout maze)) neighbors

reveal :: GameState -> [Position] -> GameState
reveal gameState positions =
  gameState { discovered = nub (currentDiscovered ++ positions) }
  where
    currentDiscovered = discovered gameState

getTile :: Maze -> Position -> Maybe Tile
getTile maze (rowIdx, colIdx)
  | inBounds grid (rowIdx, colIdx) = Just (columns `at` colIdx)
  | otherwise                      = Nothing
  where
    columns = grid `at` rowIdx
    grid = layout maze
    at      = (!!)

{------------|
|-- Render --|
|------------}
renderMap :: GameState -> String
renderMap gs@(GameState grid pos dir disc _) =
  let grid = layout $ maze gs
  in concatMap (renderRow pos dir disc) (zip [0..] grid)

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
    renderRoomWith c = "[" ++ colorizeTile [c] ++ "]"

    colorizeTile :: String -> String
    colorizeTile [c]
      | c `elem` directions = success colors ++ [c] ++ reset colors
      | c `elem` portals    = info colors ++ [c] ++ reset colors
      | c == '#'            = danger colors ++ [c] ++ reset colors
      | otherwise           = [c]
      where
        directions = ("^>v<_" :: String)
        portals    = ("SO" :: String)

playerSymbol :: Direction -> Char
playerSymbol North = '∧'
playerSymbol East  = '>'
playerSymbol South = '∨'
playerSymbol West  = '<'

tileSymbol :: Tile -> Char
tileSymbol Wall  = '#'
tileSymbol Empty = '_'
tileSymbol Start = 'S'
tileSymbol Goal  = 'O'

{------------------------------|
|-- State-Monad Based Action --|
|------------------------------}
moveForwardAction ::  Game Bool
moveForwardAction = do
  gs <- get
  let (canMove, newGameState) = moveForward gs
  put newGameState
  pure canMove

turnLeftAction :: Game ()
turnLeftAction = do
  gs <- get
  let newDir       = turnLeft $ direction gs
      newGameState = gs { direction = newDir }
  put newGameState

turnRightAction :: Game ()
turnRightAction = do
  gs <- get
  let newDir       = turnRight $ direction gs
      newGameState = gs { direction = newDir }
  put newGameState

turnDirectionAction :: Direction -> Game ()
turnDirectionAction dir = do
  gs <- get
  let newGameState = gs { direction = dir }
  put newGameState

lookAroundAction :: Game String
lookAroundAction = do
  gs <- get
  let (description, newGameState) = lookAround gs
  put newGameState
  pure description

renderMapAction :: Game String
renderMapAction = gets renderMap

{---------------|
|-- Game Loop --|
|---------------}
gameLoop :: Game ()
gameLoop = do
  liftIO $ putStr $ danger colors ++ "➜ \ESC[34m"
  liftIO $ hFlush stdout
  command <- liftIO getLine
  case parseInput command of
    Nothing -> gameLoop

    Just Quit -> handleQuit
    Just action -> do
      message <- handleAction action
      liftIO $ putStrLn message
      gameState <- get
      if arrivedGoal gameState
        then liftIO (putStrLn $ success colors ++ "🎉 You reached the goal! 🎉" ++ reset colors)
        else gameLoop


handleSequence :: [Action] -> Game String
handleSequence []  = pure ""
handleSequence [a] = handleAction a
handleSequence (a: as) = do
  message1 <- handleAction a
  gameState <- get
  if arrivedGoal gameState
    then pure message1
    else do
      message2 <- handleSequence as
      pure (message1 ++ "\n" ++ message2)

handleAction :: Action -> Game String
handleAction action = case action of
  Look               -> handleLook
  Map                -> handleMap
  Forward            -> handleMoveForward
  TurnLeft           -> handleTurnLeft
  TurnRight          -> handleTurnRight
  Turn dir           -> handleTurnDirection dir
  Help               -> handleHelp
  Sequence xs        -> handleSequence xs
  Repeat n action    -> handleRepeat n action
  Assign name action -> handleAssign name action
  Use name           -> handleUse name
  Unknown cmd        -> handleUnknown cmd

--

handleUse :: Alias -> Game String
handleUse name = do
  env <- gets aliases
  case lookup name env of
    Nothing     -> pure $ warning colors ++ ("Unknown alias: \"" ++ name ++ "\"") ++ reset colors
    Just action -> handleAction action

handleAssign :: Alias -> Action -> Game String
handleAssign name action
  | isReservedWord name =
      pure $ danger colors ++ ("error: \"" ++ name ++ "\" is reserved and cannot be redefined.") ++ reset colors
  | containsUnknown action =
      pure $ warning colors ++ "Alias contains unknown commands." ++ reset colors
  | otherwise = do
      modify (\gs -> gs { aliases = (name, action) : aliases gs })
      pure $ success colors ++ ("Alias: \"" ++ name ++ "\" defined.") ++ reset colors

reservedWords :: [String]
reservedWords =
  [ "forward", "move", "turn", "left", "right"
  , "north", "east", "south", "west"
  , "look", "map", "help", "commands"
  , "quit", "then", "use", "repeat"
  ]

isReservedWord :: String -> Bool
isReservedWord word = normalize word `elem` reservedWords
  where
    normalize = map toLower

containsUnknown :: Action -> Bool
containsUnknown action = case action of
  Unknown _   -> True
  Sequence xs -> any containsUnknown xs
  Repeat _ a  -> containsUnknown a
  Assign {}   -> False
  Use {}      -> False
  _           -> False

--

handleRepeat :: Int -> Action -> Game String
handleRepeat n action =
  go n ""
  where
    go 0 message = pure message
    go k message = do
      newMessage <- handleAction action
      gameState <- get
      if arrivedGoal gameState
        then pure (message ++ newMessage)
        else go (k - 1) (message ++ newMessage ++ "\n")

handleLook :: Game String
handleLook = lookAroundAction

handleMap :: Game String
handleMap = renderMapAction

handleMoveForward :: Game String
handleMoveForward = do
  isWalkable <- moveForwardAction
  if isWalkable
    then do
      description <- lookAroundAction
      pure $ success colors ++ "You moved forward.\n" ++
        info colors ++ description ++ reset colors
    else
      pure $ danger colors ++ "You hit the wall." ++ reset colors

handleTurnLeft :: Game String
handleTurnLeft = do
  turnLeftAction
  description <- lookAroundAction
  pure $ info colors ++ "You turned left.\n" ++ description ++ reset colors

handleTurnRight :: Game String
handleTurnRight = do
  turnRightAction
  description <- lookAroundAction
  pure $ info colors ++ "You turn right.\n" ++ description ++ reset colors

handleTurnDirection :: Direction -> Game String
handleTurnDirection dir = do
  turnDirectionAction dir
  description <- lookAroundAction
  pure $ info colors ++ "You now face " ++ show dir ++ ".\n"
    ++ description ++ reset colors

handleHelp :: Game String
handleHelp = pure helpText

handleQuit :: Game ()
handleQuit = liftIO $ putStrLn $ success colors ++ "Goodbye!"

handleUnknown :: String -> Game String
handleUnknown command =
  pure (warning colors
    ++ "Unknown command: \""
    ++ command
    ++ "\". Try: forward | turn left | turn right | look | map | help | quit"
    ++ reset colors)

helpText :: String
helpText = unlines
  [ "Available commands:"
  , "\tforward / move forward     - move ahead"
  , "\tturn left / left           - turn left"
  , "\tturn right / right         - turn right"
  , "\tturn north/south/east/west - face that direction"
  , "\tlook                       - look around"
  , "\tmap                        - show explored map"
  , "\thelp / command             - show this list"
  , "\tquit                       - exit the game"
  ]

{---------------------|
|----- IO Stuffs -----|
|---------------------}
printWelcomeMessage :: IO ()
printWelcomeMessage = do
  putStrLn "Welcome to the maze!"
  putStrLn "Type \"help\" or \"command\" to see available commands"

printUsage :: String -> IO ()
printUsage programName = do
  putStrLn $ "Usage " ++ programName ++ " <maze_file>"
  putStrLn $ "Example: " ++ programName ++ " maze-01.txt"

startGameFrom :: [String] -> IO ()
startGameFrom [path] = do
  mazeGrid <- loadMaze path
  either printError startGame mazeGrid
startGameFrom _ = getProgName >>= printUsage

loadMaze :: FilePath -> IO (Either ErrorMessage Maze)
loadMaze path = do
  mazeGrid <- readFile path
  case runParser parseMap mazeGrid of
    Just (maze, _) -> pure $ Right maze
    Nothing        -> do
      pure . Left $ "Failed to parse maze file \"" ++ path ++ "\". Please check file format."

printError :: String -> IO ()
printError err = putStrLn $ "Error: " ++ err

startGame :: Maze -> IO ()
startGame maze = do
  let game = newGame maze
  printWelcomeMessage
  evalStateT gameLoop game

{----------|
|-- Main --|
|----------}
main :: IO ()
main = do
  test
  testParse

  putStrLn (info colors)
  putStrLn "------------------------------"
  putStrLn "--------- Start Game ---------"
  putStrLn "------------------------------"
  mazeStage <- getArgs
  printWelcomeMessage
  putStrLn $ reset colors

  startGameFrom $ map mazePath mazeStage

{-----------|
|-- Misc. --|
|-----------}
capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : map toLower xs

mazePath :: String -> String
mazePath = ("../maze-maps/maze-" ++) . (++ ".txt") . padZero
  where
    padZero [c] = ['0', c]
    padZero s   = s

{-------------------------|
|--- Dummy (Test Data) ---|
|-------------------------}
testMazeInput :: String
testMazeInput = "[x][x][x][x][x][x][x][x]\n[x][x][x][_][_][x][x][x]\n[s][_][_][_][x][x][o][x]\n[x][x][x][_][x][x][_][x]\n[x][x][x][_][_][_][_][x]\n[x][x][x][x][x][x][x][x]\n"

testMaze :: Maze
testMaze = runParserUnsafe parseMap testMazeInput

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

testParse :: IO ()
testParse = do
  putStrLn "\n------------------------------"
  putStrLn "--- Testing Action Parser  ---"
  putStrLn "------------------------------"

  let testCases =
        -- 1. Movement
        [ ("forward",       Just Forward)  -- Happy path
        , ("move forward",  Just Forward)  -- Synonym
        , ("turn left",     Just TurnLeft) -- Two words

        -- 2. Turning (Relative)
        , ("turn left",     Just TurnLeft)
        , ("left",          Just TurnLeft)
        , ("turn right",    Just TurnRight)
        , ("right",         Just TurnRight)

        -- 3. Turning (Absolute / Cardinal)
        , ("turn north",    Just (Turn North))
        , ("north",         Just (Turn North))
        , ("turn south",    Just (Turn South))
        , ("east",          Just (Turn East))
        , ("WEST",          Just (Turn West)) -- Case Insensitivity Check

        -- 4. Actions / Utilities
        , ("look",          Just Look)
        , ("map",           Just Map)
        , ("help",          Just Help)
        , ("command",      Just Help)      -- Synonym
        , ("quit",          Just Quit)

        -- 5. Invalid Commands (Should fail)
        , ("fly",           Nothing)        -- Unknown command
        , ("jump",          Nothing)
        , ("turn up",       Nothing)        -- Invalid direction
        , ("",              Nothing)        -- Empty string
        , ("   ",           Nothing)        -- Whitespace only
        ]

  mapM_ check testCases

  where
    check (input, expected) = do
      let result = runParser parseAction input
      let actual = fmap fst result

      let status = if actual == expected
                   then "✅ PASS"
                   else "❌ FAIL"

      putStrLn $ status ++ " Input: " ++ show input
               ++ " -> Expected: " ++ show expected
               ++ ", Actual: " ++ show actual
