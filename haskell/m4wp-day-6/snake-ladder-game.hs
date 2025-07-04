module SnakeLadderGame where

import Data.Maybe (fromMaybe)
import Data.List (find)
import qualified Data.Map.Strict as Map

-- Domain Models
data Game = SnakeLadder { board :: Board
                        , players :: [PlayerPosition]
                        , status :: GameStatus
                        }
                        deriving Show

data Board = Board { goal :: Position
                   , portals :: [Portal] }
                   deriving Show

type Position = Int
type PlayerPosition = (Player, Position)
type Portal = (Position, Position)

newtype Player = Player Int
  deriving (Eq, Show)

data PlayerMove = PlayerMove { player :: Player
                             , move :: Int }

data GameStatus = WonBy Player | Playing
  deriving (Eq, Show)

applyMove :: Game -> PlayerMove -> Game
applyMove game@(SnakeLadder board playerPositions gameStatus) playerMove@(PlayerMove movingPlayer steps)
  | gameStatus /= Playing = game -- Game already ended
  | otherwise =
    let updatedPlayers = map (updatePlayerIfMatch board playerMove) playerPositions
        newStatus = if hasPlayerWon board updatedPlayers movingPlayer
                    then WonBy movingPlayer
                    else Playing
     in SnakeLadder board updatedPlayers newStatus

updatePlayerIfMatch :: Board -> PlayerMove -> PlayerPosition -> PlayerPosition
updatePlayerIfMatch board (PlayerMove targetPlayer steps) (currentPlayer, currentPos)
  | currentPlayer == targetPlayer = (currentPlayer, resolvePortal board (currentPos + steps))
  | otherwise = (currentPlayer, currentPos)

resolvePortal :: Board -> Position -> Position
resolvePortal (Board _ portals) currentPos = fromMaybe currentPos (Map.lookup currentPos portalMap)
  where
    portalMap = Map.fromList portals

hasPlayerWon :: Board -> [PlayerPosition] -> Player -> Bool
hasPlayerWon (Board goalPos _) playerPositions targetPlayer =
  case lookup targetPlayer playerPositions of
    Just playerPos -> playerPos >= goalPos
    Nothing        -> False

initialGame :: Board -> [Player] -> Game
initialGame board players = SnakeLadder board [(player, 0) | player <- players] Playing

sampleBoard :: Board
sampleBoard = Board { goal = 100
                    , portals = [(3, 22), (5, 8), (11, 26), (20, 29), (27, 1), (21, 9), (17, 4)]
                    }

samplePlayers :: [Player]
samplePlayers = [Player 1, Player 2]

game0 :: Game
game0 = initialGame sampleBoard samplePlayers

game1 :: Game
game1 = applyMove game0 (PlayerMove (Player 1) 3) -- lands on 3 -> portal to 22

game2 :: Game
game2 = applyMove game1 (PlayerMove (Player 2) 5) -- lands on 5 -> portal to 8
