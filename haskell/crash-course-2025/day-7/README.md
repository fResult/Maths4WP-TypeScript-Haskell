# Day 7 - Haskell Maze Game - Version 3

This is Version 3, refactored from Version 2:

- `MazeV3.hs` is the new version of the maze game.\
  It is currently being built and tested.

## Refactoring Notes

We are breaking down `parseMap` (from `MazeV2.hs`) into smaller parsers to replace manual string manipulation with combinators.

**What:** Renamed `type Maze` to `MazeLayout` and introduced a `newtype Maze` wrapper.
**Why:** To decouple the raw grid data from the domain model. This prepares the `Maze` type to hold future pre-computed properties (e.g., shortest route, maze complexity) without breaking existing layout logic.

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

