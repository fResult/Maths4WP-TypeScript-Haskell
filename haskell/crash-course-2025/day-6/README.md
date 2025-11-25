# Day 6 - Haskell Maze Adventure Game

- **`maze-v1.hs`**: The stable baseline version.\
  Implements the game using standard list manipulation and jagged array handling
- **`maze-v2.hs`**: The development version.
  Currently being refactored to include a custom `Parser` type and instances (`Functor`, `Applicative`, `Monad`, `Alternative`)

## How to Run

### Version 1 (Stable / Baseline)

This version runs the original logic (Manual List Processing).

```bash
cd $(git rev-parse --show-toplevel) && \
  ghci haskell/crash-course-2025/day-6/maze-v1.hs
```

**Inside GHCi**:

```bash
main
-- Or to run unit tests:
test
```

### Version 2 (Functional Parser)

This version contains the new `Parser` type definitions and is under active development.

```bash
cd $(git rev-parse --show-toplevel) && \
  ghci haskell/crash-course-2025/day-6/maze-v2.hs
```

**Inside GHCi**:

```bash
main
-- Or to run unit tests:
test
```
