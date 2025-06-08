-- Domain Models
type Position = Int
type PlayerPosition = (Player, Position)
type Portal = (Position, Position)

newtype Player = Player Int

data PlayerMove = PlayerMove { player :: Player
                             , move :: Int }

data Board = Board { goal :: Position
                   , portals :: [Portal] }

data GameStatus = WonBy Player | Playing

data Game = SnakeLadder { board :: Board
                        , players :: [PlayerPosition]
                        , status :: GameStatus
                        }
