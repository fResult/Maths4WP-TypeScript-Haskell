# Day 8 - Building a DSL with AST and Monadic Parsers

## The Big Picture (TL;DR)

Day 8 is a massive architectural leap.\
We are transitioning our game from a simple command-driven system into a programmable platform. 

By elevating string inputs into an **Abstract Syntax Tree (AST)**, we successfully decouple the "What" (Parsing) from the "How" (Interpretation).\
This zoomed-out architectural shift allows us to build a robust **Domain-Specific Language (DSL)**.

**The Journey at a Glance:**
- **Phase 1 (MazeV6):** Establishing the AST foundation and completing the Interpreter Pattern.
- **Phase 2 (MazeV7):** Introducing Graceful Degradation to handle invalid states without crashing.
- **Phase 3 (MazeV8):** Expanding the DSL's expressiveness (loops, flexible syntax) while keeping the core domain pure.

---

## 1. Architectural Foundations

Before diving into the DSL construction, let's step back and look at the key architectural lessons from Day 7 that make this possible.

### The "Functional Core, Imperative Shell" Architecture

The architecture of our [MazeV5][maze-v5] game uses the **Functional Core, Imperative Shell** pattern.\
This design separates pure logic from side-effects.

- **The Functional Core:** This is our pure domain logic (`s` in `StateT s m a`).
  - Game mechanics (like `moveForwardAction`) are pure functions transforming this state.
  - **Benefit:**\
    The core is 100% testable in isolation.\
    We can test it in the REPL without mocking databases or consoles.

- **The Imperative Shell:** This handles side-effects (`m` in `StateT s m a`, our `IO` monad).
  - The `gameLoop` is the shell.\
    It uses `liftIO` for real-world IO like `getLine` and `putStrLn`.
  - **Benefit:**\
    Messy parts are pushed to the edge, keeping the core predictable.

Haskell's type system naturally guides us to this clean architecture.\
Other languages **require extreme discipline to avoid mixing logic with I/O**.

### Separating "What" From "How"

A key takeaway is separating *describing* an action from *executing* it.

1. **Parsing (The "What"):**\
When a user types "forward", `parseAction` does not move the player.\
It creates a `Forward :: Action` data value describing intent.

2. **Interpretation (The "How"):**\
The `handleAction` function takes this `Forward` value and interprets it.\
It executes pure state-changing logic (`moveForwardAction`).

This separation lets us delay or modify execution.\
This is the foundation for programmable systems, our goal for Day 8.

## 2. Day 8 Goals & Philosophy

### The Goal: Building a Mini Language (AST)

We will upgrade our `Action` type into an **Abstract Syntax Tree (AST)**.\
This transforms our parser into an **Interpreter** for a **Domain-Specific Language (DSL)**.

We apply the **Interpreter Pattern** by elevating inputs into a mini-language.\
This lets users script complex behaviors without changing core game logic.

Our goal is to support commands like:
- **Sequence:** `forward then left then forward`
- **Repetition:** `repeat 3 forward` or `left 3`
- **Macros/Aliases:** `alias jump = forward 2` and then `use jump`

### Architectural Philosophy: Managing Parser Complexity

As we add features, our parser logic becomes *ad-hoc*. However, we deliberately postpone a full parser revamp.

Think of software architecture like a new house.\
Building custom shelves before buying furniture leads to wasted space.\
We must collect domain complexity gradually.

We will refactor the parser only *after* understanding our DSL's full scope.\
Premature abstraction is the root of all evil.

## 3. Upgrading the Toolkit (The Prerequisites)

Before building our DSL, we need better parsing tools.

### [ParserV3][parser-v3] (Monadic Parser)

To support complex syntax, we upgraded our custom `Parser` to be a full Monad.

- **What:** Implemented the `Monad` type class instance (`>>=`) for our `Parser`.
- **Why:** To unlock `do` notation. `Applicative` (`<*>`) handles independent parsing, but `Monad` allows context-dependent parsing. It makes complex syntax rules cleaner.

### [ParserV4][parser-v4] (Predicate Combinators)

- **What:** Extracted `satisfy :: (Char -> Bool) -> Parser Char` and refactored `char c = satisfy (== c)`.
- **Why:** To create a reusable combinator that parses characters based on any predicate. This is crucial for parsing arbitrary text.

### [ParserV5][parser-v5] (Foundational Combinators)

- **What:** Added `parseInt :: Parser Int` and `between :: Parser a -> Parser b -> Parser c -> Parser c`.
- **Why:** To build reusable primitives. `between` creates scoped syntax boundaries (like parentheses) without cluttering business logic.

## 4. Phase 1: The Foundation - AST & Interpreter ([MazeV6][maze-v6])

We begin our DSL journey by introducing the first composite node into our AST.\
This is where the Interpreter Pattern comes to life.

### 4.1 Expanding the AST (The Model)

- **What:** Added `Sequence [Action]` to the `Action` data type.
- **Why:** To elevate our domain model from isolated commands to complex, programmable structures. `Sequence` holds a list of sub-actions to execute in order.

### 4.2 Parsing the Language Syntax (The "What")

We created a parser that understands how to chain commands together.

- **What:** Introduced `parseSequence :: Parser Action` using `many` and `do` notation. Updated `parseInput` to use it as the root parser.
- **Why:** To parse strings (e.g., `"forward then left"`) into a `Sequence` AST node.
- **Demo:** Notice how it handles both single actions and chained sequences:
  ```hs
  -- Single action still works natively
  λ > parseInput "forward"
  Just Forward

  -- Chained actions are combined into an AST node
  λ > parseInput "forward then turn left then forward"
  Just (Sequence [Forward,TurnLeft,Forward])
  ```

### 4.3 Interpreting the AST (The "How")

We upgraded our game's core action handler from a simple switch-case into a recursive AST Interpreter.

- **What:** Enhanced `handleAction` with pattern matching for the `Sequence` node and introduced `handleSequence :: [Action] -> Game String` to recursively evaluate the list of actions.
- **Why:** To complete the **Interpreter Pattern**. The parser builds the AST, and the interpreter reduces it into pure state mutations.
- **Why (cont.):** We also implemented **Short-Circuit Evaluation**. If the player reaches the goal mid-sequence, execution stops immediately.
- **Demo:** Notice how the interpreter evaluates multiple actions sequentially and aggregates the results:
  ```hs
  λ > evalStateT (handleSequence [Forward, Turn South]) testGame
  "\ESC[32mYou moved forward.\n\ESC[36m\ESC[36mYou see a path in front of you. A wall to the left. A wall to the right.\ESC[m\ESC[m\n\ESC[36mYou now face South.\n\ESC[36mYou see a wall in front of you. A path to the left. A path to the right.\ESC[m\ESC[m"
  ```

## 5. Phase 2: Resilience - Graceful Degradation ([MazeV7][maze-v7])

### Architectural Insight: The Universal Pattern of Decoupling

Zooming out to a macro perspective, let's look at a larger architectural concept.\
In traditional synchronous Client-Server architecture, invoking an API immediately triggers computation and database queries, which inherently blocks scalability.\
To solve this in large-scale Distributed Systems, Engineers often introduce a **Message Queue** in the middle.\
Technically, this is the concept of **decoupling IO (Accepting the Request) from Computation (Processing the Request)**.

This is *exactly* the same concept as our Parser and Interpreter separation!\
The Parser acts like the API Gateway accepting the request and putting it into an AST (the Message Queue).\
The Interpreter then pulls that AST to handle the state transition (the Computation).\
Functional Architecture naturally guides us toward these highly scalable, decoupled design patterns!

### 5.1 Parser Refactoring ([`ParserV4.hs`][parser-v4])

- **What:** Extracted the `satisfy :: (Char -> Bool) -> Parser Char` function and refactored the `char` parser to use it (`char c = satisfy (== c)`).
- **Why:** To create a foundational, highly reusable combinator that parses characters based on *any* predicate. This is a crucial tool for parsing arbitrary text blocks without writing custom recursive loops.

### 5.2 Enhancing the Domain for UX ([`MazeV7.hs`][maze-v7])

- **What:** Added `Unknown String` to the `Action` AST, introduced `parseUnknown` using the new `satisfy` combinator, and updated the interpreter (`handleAction` & `handleUnknown`) to accept this string.
- **Why:** Instead of discarding invalid input during parsing, we capture it into our Domain Model (`Unknown`). This achieves **Graceful Degradation**.
- **Why (cont.):** In Distributed Systems, this mirrors routing failed messages to a **Dead Letter Queue (DLQ)**. The Interpreter (Worker) can then inspect the faulty command and provide a tailored UX instead of a hard crash.

For example, we could implement Levenshtein distance later to offer suggestions: `"Did you mean 'forward' instead of 'forwrd'?"`.

> [!NOTE]
> We will skip the actual *Levenshtein* implementation in this course, but the architectural foundation is now ready for it!)

## 6. Phase 3: Expressiveness - Advanced AST ([MazeV8][maze-v8])

### Architectural Philosophy: Managing Parser Complexity

As our DSL grows (`Sequence`, `Repeat`), our parser logic naturally becomes slightly *ad-hoc*.\
However, we are deliberately postponing a full parser revamp.\
Think of software architecture like moving into a new house.\
If you build custom shelves before knowing what furniture you'll buy, you'll end up with mismatched spaces.\
We must collect our domain complexity gradually, understand our usage patterns, and *then* refactor to reduce entropy.\
Premature abstraction is the root of all evil.

### 6.1 Foundational Combinators ([ParserV5][parser-v5])

- **What:** Added `parseInt :: Parser Int` and `between :: Parser a -> Parser b -> Parser c -> Parser c`.
- **Why:** To build highly reusable parser primitives. `between` is particularly powerful for creating scoped syntax boundaries (like parentheses) without cluttering the business logic.

### 6.2 Expanding the Language Syntax ([MazeV8][maze-v8])

- **What:** Added `Repeat Int Action` to the `Action` AST.
- **Why:** To introduce looping constructs into our DSL. We explicitly chose **Prefix Notation** (`repeat 3 forward`) over postfix (`forward 3`) to align with standard Unix CLI ergonomics (e.g., `repeat 3 { echo "hello" }`), making the language intuitive for developers.

### 6.3 The Composite Parser Refactoring

- **What:** Renamed the base `parseAction` to `parseAtomicAction`. Created a new, higher-level `parseAction` that handles composite rules (`parseRepeatPrefix <|> parseAtomicAction <|> parseUnknown`), and introduced `parseParenthesesOrAction`.
- **Why:** To separate terminal nodes (atomic operations like `Forward`) from composite nodes (like `Repeat` or `Sequence`). This prevents infinite recursion in parsing and enables the evaluation of nested, complex expressions.
- **Demo:** Notice how the AST easily represents nested logic:
  ```hs
  -- Flat AST node
  λ > parseInput "repeat 3 forward"
  Just (Repeat 3 Forward)
  
  -- Nested AST nodes thanks to `parseParenthesesOrAction`
  λ > parseInput "repeat 3 (forward then left)"
  Just (Repeat 3 (Sequence [Forward, TurnLeft]))
  ```

### 6.4 Syntax Flexibility (Prefix & Postfix)

We expanded the language to support both Prefix and Postfix notations, mapping them to the exact same AST node.

- **What:** Implemented `parseRepeatPostfix` (e.g., `forward 3`) and enabled it alongside `parseRepeatPrefix` in the main `parseAction`.
- **Why:** To improve the ergonomics (UX) of our DSL. Some users prefer Unix-style prefix (`repeat 3 forward`), while others prefer object-like postfix (`forward 3`).
- **Why (cont.):** Because Parsing is decoupled from Interpretation, both syntaxes map to the *identical* `Repeat Int Action` AST node. We expanded expressiveness without *any* burdening the core interpreter!
- **Demo:** Notice how postfix syntax elegantly resolves to the same AST:
  ```hs
  λ > parseInput "forward 3"
  Just (Repeat 3 Forward)
  λ > parseInput "(forward then left) 3"
  Just (Repeat 3 (Sequence [Forward, TurnLeft]))
  ```

### 6.5 Interpreting the Repeat Node *(WIP/Pending)*

*We have parsed the "What", but the Interpreter (`handleAction`) is currently missing the "How" for the `Repeat` node. We need to implement this next!*

[maze-v5]: ../day-7/MazeV5.hs
[maze-v6]: ./MazeV6.hs
[maze-v7]: ./MazeV7.hs
[maze-v8]: ./MazeV8.hs
[parser-v2]: ./ParserV2.hs
[parser-v3]: ./ParserV3.hs
[parser-v4]: ./ParserV4.hs
