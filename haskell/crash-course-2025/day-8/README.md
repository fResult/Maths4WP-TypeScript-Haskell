# Day 8 - Maze Game with Mini Language (AST)

## Summary of the Day 7

This summary is the key architectural lesson from Day 7  and sets the stage for Day 8, where we will build a mini language for our game.

### The "Functional Core, Imperative Shell" Architecture

The final architecture of our [MazeV5](../day-7/MazeV5.hs) game is a classic pattern called **Functional Core, Imperative Shell**.\
This design separates pure logic from side-effects.

- **The Functional Core:** This is our pure domain logic, represented by the `s` in `StateT s m a` (our `GameState`)
  - All game mechanics (like `moveForwardAction`) are pure functions that only transform this state.
  - **Benefit:**\
    This core is 100% testable isolation.\
    We can test it in the REPL without needing to mock databases or consoles.\
    This is often called the "REPL Test" for healthy codebase.
- **The Imperative Shell:** This is where side-effects happen, represented by the `m` in `StateT s m a` (our `IO` monad).
  - The `gameLoop` is the main part of our shell.\
    It uses `liftIO` to handle real-world interactions like `getLine` and `putStrLn`.
  - **Benefit:**\
    All the "messy" parts of the program are pushed to the very edge, keeping the core clean and predictable.

Haskell's type system naturally guides us toward this clean architecture, whereas other languages often **require more discipline to avoid mixing logic with I/O**.

## Refactoring Note

Coming Soon...
