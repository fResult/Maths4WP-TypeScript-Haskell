# Day 7 - Haskell Maze Game - Version 3

There are the version 3 which is refactored from version 2:

- `MazeV3.hs` is the new version of the maze game.\
  It is currently being built and tested.

## Refactoring Notes

We are breaking down `parseMap` (from `MazeV2.hs) into smaller parsers to replace manual string manipulation with combinators.\
This leads to more focused functions to improve readability, testability, and maintainability.

- **What:** Extracted the tile parsing logic into a new standalone function: `parseTile :: Parser Tile`.
- **Why:** To replace imperative string slicing with a declarative approach. This makes the parsing logic composable and easier to test in isolation.

```hs
λ > runParser parseTile "[x]"
Just (Wall,"")

λ > runParser parseTile "[_]"
Just (Empty,"")

λ > runParser parseTile "[ ]"
Nothing
```

- **What:** Introduced `parseRow` using `some` applicative combinator (`some parseTile`).
- **Why:** To automatically parse a sequence of tiles until the end of the row. It aggregates results into a list of tile (`[Tile]`) without requiring manual loops.

```hs
λ > runParser parseRow "[s][_][_][_][x][x][o][x]"
Just ([Start,Empty,Empty,Empty,Wall,Wall,Goal,Wall],"")

-- Stops parsing when it encounters an unknown tile (e.g., "[?]")
λ > runParser parseRow "[s][_][?][_][x][x][o][x]"
Just ([Start,Empty],"[?][_][x][x][o][x]")
```
