# Day 7 - Haskell Maze Game - Version 3

This is Version 3, refactored from Version 2:

- `MazeV3.hs` is the new version of the maze game.\
  It is currently being built and tested.

## Refactoring Notes

### Maze Version 3 (Parser)

We are breaking down `parseMap` (from `MazeV2.hs`) into smaller parsers to replace manual string manipulation with combinators.

- **What:** Renamed `type Maze` to `MazeLayout` and introduced a `newtype Maze` wrapper.
- **Why:** To decouple the raw grid data from the domain model. This prepares the `Maze` type to hold future pre-computed properties (e.g., shortest route, maze complexity) without breaking existing layout logic.

- **What:** Extracted the tile parsing logic into a new standalone function: `parseTile :: Parser Tile`.
- **Why:** To replace imperative string slicing with a declarative approach. This makes the parsing logic composable and easier to test in isolation.
- **Demo:**
  ```hs
  λ > runParser parseTile "[x]"
  Just (Wall,"")

  λ > runParser parseTile "[_]"
  Just (Empty,"")

  λ > runParser parseTile "[ ]"
  Nothing
  ```

- **What:** Introduced `parseRow` using `some` applicative combinator (`some parseTile`).
- **Why:** To automatically parse a sequence of tiles until the end of the row. It aggregates results into a list of `Tile` (`[Tile]`) without requiring manual loops.
- **Demo:**

  ```hs
  λ > runParser parseRow "[s][_][_][_][x][x][o][x]"
  Just ([Start,Empty,Empty,Empty,Wall,Wall,Goal,Wall],"")

  -- Stops parsing when it encounters an unknown tile (e.g., "[?]")
  λ > runParser parseRow "[s][_][?][_][x][x][o][x]"
  Just ([Start,Empty],"[?][_][x][x][o][x]")
  ```

- **What:** Introduce `parseByLines :: Parser a -> Parser [a]` to run a parser across multiple lines.
- **Why:** To parse the entire maze map row-by-row-combining the results into a list of rows (`[[Tile]]`).

- **What:** Introduced `parseMaze :: Parser Maze` as the final composer.
- **Why:** To combine `parseByLines parseRow` and map the result directly into the `Maze` domain object (`Maze <$> ...`), completing the replacement of the old `parseMap`.

- **What:** Added `processLine` helper inside `ParserV2.hs` to filter out carriage returns (`\r`) and handle trailing whitespaces/tabs.
- **Why:** To make the parser robust against different operating system line endings (CRLF vs LF) and invisible trailing spaces in text files.
- **Demo:**
  ```hs
  λ > runParser parseMaze testMazeInput
  Just (Maze { layout = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall],[Start,Empty,Empty,Empty,Wall,Wall,Goal,Wall],[Wall,Wall,Wall,Empty,Wall,Wall,Empty,Wall],[Wall,Wall,Wall,Empty,Empty,Empty,Empty,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]},"")
  ```

- **What:** Introduced \`loadMaze :: FilePath -> IO (Either ErrorMessage Maze)\` to handle parsing logic at the I/O boundary.
- **Why:** To handle invalid file formats. By converting the \`Parser\`'s \`Nothing\` state into an \`Either ErrorMessage Maze\`, the application can gracefully report parsing error instead of crashing or silently failing.

### Maze Version 4 (State)

We refactored the game logic to use a `State` monad for managing the game state instead of manually threading it through every function.

- **What:** Introduced a state monad alias `type Game a = State GameState a` and extracted `updateVision`.
- **Why:** To eliminate the boilerplate of manually passing `GameState` and to encapsulate the domain concept of "Fog of War" (`updateVision`), keeping the business logic low-entropy and readable.

- **What:** Extracted primitive state operations (e.g., `moveForward :: Game Bool`, `turnLeftAction :: Game ()`) utilizing `get` and `put` from the `State` monad.
- **Why:** To strictly separate core state mutations from the presentation formatting. By building a composition internal DSL for game mechanics, the command handlers (like `handleMoveForward`)

- **What:** Refactored action handlers (e.g., `handleAction`, `handleMoveForward`) to return `Game String` instead of `GameState -> IO GameState`.
- **Why:** To push side-effects (`IO`) to the outermost boundary of the system (`gameLoop`). By decoupling pure state transitions from I/O operations, the core game logic becomes perfectly pure, deterministic, and highly testable without needing to mock the console.
- **Demo:**
  ```hs
  -- 1. Initialize a pure state
  λ > let initialState = newGame testMaze

  -- 2. Run pure state transitions without IO side-effects
  λ > let (outputMsg, nextState) = runState handleLook initialState

  λ > outputMsg
  "You see a path in front of you. A wall to the left. A wall to the right."
  ```

## Minor Enhancement

- Added basic ANSI terminal colors and updated map symbols (e.g., 'S' for Start, 'O' for Goal) for better readability.
- Improved CLI argument handling to accept a simple maze run

## How to Play

Run the game and optionally pass a maze number (defaults to `1`):

```bash
./run-maze-v3.md [maze_number]
# OR if you are in the root folder:
haskell/crash-course-2025/day-7/run-maze-v3.md [maze_number]
```


*Example:* `./run-maze-v3.md 2` (loads `maze-02.txt`)
*(From the project root: `haskell/crash-course-2025/day-7/run-maze-v3.md 2`)

**To quit the game:** Press <kbd>Ctrl</kbd> + <kbd>C</kbd>.

## Development Notes

Boot up GHCi with a specific maze loaded (defaults to `1`):

```bash
./develop-maze-v3.md [maze_number]
# OR if you are in the root folder:
haskell/crash-course-2025/day-7/develop-maze-v3.md [maze_number]
```

To change the maze on the fly, use the `:set args` command in GHCi:

```hs
:set args <maze_number>
-- OR
:main <maze_number>
```
