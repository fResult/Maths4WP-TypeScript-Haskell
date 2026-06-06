# Day 8 - Building a DSL with AST and Monadic Parsers

## Summary of the Day 7 & Day 8 Kick-off

This summary is the key architectural lesson from Day 7  and sets the stage for Day 8, where we will build a mini language for our game.

### The "Functional Core, Imperative Shell" Architecture

The final architecture of our [MazeV5][maze-v5] game is a classic pattern called **Functional Core, Imperative Shell**.\
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

### Separating "what" From "How"

A key takeaway from our journey is the separation between *describing* an action and *executing* it.

1. **Parsing (The "What"):**\
When a user types "forward", our `parseAction` function doesn't move the player.\
It creates a data value `Forward :: Action`.\
This is just a description of the user's intent.
2. **Interpretation (The "How")**\
The `handleAction` function then takes this `Forward` value and *interprets* it, running the actual state-changing logic (`moveForwardAction`).

This separation allows us to delay or even change how an action is executed.\
This is the foundation for building more complex, programmable systems, which is our goal for Day 8.

### Day 8 Goal: Building a Mini Language (AST)

Now that we have a solid architecture, we will upgrade our `Action` data type into an **Abstract Syntax Tree (AST)**.\
This transforms our simple command parser into an **Interpreter** for a **Domain-Specific Language (DSL)**.

By elevating our inputs into a mini-language (non-Turing complete), we apply the **Interpreter Pattern**. This is a practical stepping stone into Programming Language Design, allowing users to script complex behaviors without modifying the core game logic.

Our goal is to support commands like:

- **Sequence:** `forward then left then forward`
- **Repetition:** `repeat 3 forward` or `left 3`
- **Macros/Aliases:** `alias jump = forward 2` and then `use jump`

This will evolve:
1. **Expanding the `Action` data type** to include these new structures.
2. **Enhancing `parseAction`** to understand this new, more complex syntax.
3. **Upgrading `handleAction`** to recursively interpret the AST.

### Future Scope: Parser Revamp (The "House Organization" Analogy)

As we incrementally add features, our parser has become somewhat *ad-hoc*.\
Continuing to patch it with "duct tape" will eventually lead to spaghetti code.\
In software architecture, this is like moving into a new empty house.\
At first, you can place furniture anywhere.\
Over time, as you buy more items, the house becomes cluttered.\
The most effective time to reorganize (refactor) is *after* you have lived in it for a while and truly understand your daily usage patterns.

Similarly, we should base our new parser architecture on the actual complexity we encounter.\
Therefore, we will postpone the parser revamp until we have fully finished implementing the AST and DSL.\
We must understand the full scope of our domain language before we abstract it.

Let's start by refactoring [MazeV5.hs][maze-v5] into [MazeV6.hs][maze-v6] and begin designing our AST.

## Refactoring Note

### [ParserV3][parser-v3] (Monadic Parser)

We upgraded our custom `Parser` to be a full Monad.

- **What:** Implemented the `Monad` typeclass instance (`>>=`) for our `Parser`.
- **Why:** To unlock the power of `do` notation. While `Applicative` (`<*>`) is great for independent parsing, a `Monad` allows context-dependent parsing step can depend on the extracted value of the previous step. It makes writing complex syntax rules (like our sequence parser) cleaner.

###  [MazeV6.hs][maze-v6] (AST & Interpreter)

We started building our mini language by introducing the first composite node into our Abstract Syntax Tree (AST).

#### 1. Expanding the AST (The Model)

- **What:** Added `Sequence [Action]` to the `Action` data type.
- **Why:** to elevate our domain model from single, isolated commands into a programmable structure. `Sequence` acts as an AST node that holds a list of sub-actions to be executed sequentially.

### 2. Parsing the Language Syntax (The "What")

We created a parser that understands how to chain commands together.

- **What:** Introduced `parseSequence :: Parser Action` utilizing the `many` combinator and monadic `do` notation.
- **Why:** To parse a language string (e.g., `"forward then left"`) and transform it into our new `Sequence` AST node. We also updated `parseInput` to use `parseSequence` as the root parser instead of `parseAction`.
- **Demo:** Notice how it handles both single actions and chained sequences:
  ```hs
  -- Single action still works natively
  λ > parseInput "forward"
  Just Forward

  -- Chained actions are combined into an AST node
  λ > parseInput "forward then turn left then forward"
  Just (Sequence [Forward,TurnLeft,Forward])
  ```

### 3. Interpreting the AST (The "How")

We upgraded our game's core action handler to ac as an AST Interpreter.

- **What:** Enhanced `handleAction` with pattern matching for the `Sequence` node and introduced `handleSequence :: [Action] -> Game String` to recursively evaluate the list of actions.
- **Why:** To complete the **Interpreter Pattern**. while parser builds the AST (describing "what" to do), the interpreter executes it (defining *how* to do it) by reducing the AST into state mutations. Furthermore, we implemented **Short-Circuit Evaluation**: if the player reaches the goal mid-sequence, the interpreter halts further execution, preventing unnecessary and potentially invalid state transitions.
- **Demo:** Notice how the interpreter evaluates multiple actions sequentially and aggregates the results:
  ```hs
  λ > evalStateT (handleSequence [Forward, Turn South]) testGame
  "\ESC[32mYou moved forward.\n\ESC[36m\ESC[36mYou see a path in front of you. A wall to the left. A wall to the right.\ESC[m\ESC[m\n\ESC[36mYou now face South.\n\ESC[36mYou see a wall in front of you. A path to the left. A path to the right.\ESC[m\ESC[m"
  ```

### [Maze Version 7](maze-v7) (Unknown Command Suggestion)

#### Architectural Insight: The Universal Pattern of Decoupling

Before diving into the code, let's look at a larger architectural concept.\
In traditional synchronous Client-Server architecture, invoking an API immediately triggers computation and database queries, which inherently blocks scalability.\
To solve this in large-scale Distributed Systems, Engineers often introduce a **Message Queue** in the middle.\
Technically, this is the concept of **decoupling IO (Accepting the Request) from Computation (Processing the Request)**.

This is *exactly* the same concept as our Parser and Interpreter separation!\
The Parser acts like the API Gateway accepting the request and putting it into an AST (the Message Queue).\
The Interpreter then pulls that AST to handle the state transition (the Computation).\
Functional Architecture naturally guides us toward these highly scalable, decoupled design patterns!

#### 1. Parser Refactoring ([`ParserV4.hs`][parser-v4])

- **What:** Extracted the `satisfy :: (Char -> Bool) -> Parser Char` function and refactored the `char` parser to use it (`char c = satisfy (== c)`).
- **Why:** To create a foundational, highly reusable combinator that parses characters based on *any* predicate. This is a crucial tool for parsing arbitrary text blocks without writing custom recursive loops.

#### 2. Enhancing the Domain for UX ([`MazeV7.hs`][maze-v7])

- **What:** Added `Unknown String` to the `Action` AST, introduced `parseUnknown` using the new `satisfy` combinator, and updated the interpreter (`handleAction` & `handleUnknown`) to accept this string.
- **Why:** Instead of discarding invalid input or throwing an immediate error at the parsing stage, we *capture* it into our Domain Model (`Unknown`). In Distributed Systems, this pattern is the equivalent of routing failed messages to a **Dead Letter Queue (DLQ)** to achieve **Graceful Degradation**. It pushes the responsibility to the Interpreter (the Worker), allowing it to inspect the faulty command and provide a tailored user experience instead of a hard crash.\
For example, we could implement Levenshtein distance later to offer suggestions: `"Did you mean 'forward' instead of 'forwrd'?"`.

> [!NOTE]
> We will skip the actual *Levenshtein* implementation in this course, but the architectural foundation is now ready for it!)

[maze-v5]: ../day-7/MazeV5.hs
[maze-v6]: ./MazeV6.hs
[maze-v7]: ./MazeV7.hs
[parser-v2]: ./ParserV2.hs
[parser-v3]: ./ParserV3.hs
[parser-v4]: ./ParserV4.hs
