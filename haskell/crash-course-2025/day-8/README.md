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
- **Phase 4 (MazeV9):** Programmability & Stateful Environment (Macros & Symbol Table).
- **Phase 5 (MazeV10):** Top-Down Parser Revamp & Formal Grammar.

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

### Architectural Insight: Decoupling UX from Core Logic

Zooming out to a macro perspective, supporting multiple syntaxes mirrors **Hexagonal Architecture (Ports and Adapters)**.

Our parsers act as primary Adapters.\
They translate diverse, messy user inputs into a unified format.

The AST represents our pure Core Domain.\
It remains blissfully unaware of syntax choices.

By decoupling "How it looks" (Prefix/Postfix) from "What it means" (AST), we scale our UX freely.\
We avoid bleeding UX complexity into the interpreter.

### 6.4 Parsing Flexible Syntax (Prefix & Postfix)

We expanded the language to support both Prefix and Postfix notations.\
Both syntaxes seamlessly map to the exact same AST node.

- **What:** Implemented `parseRepeatPostfix` and enabled it alongside `parseRepeatPrefix` in `parseAction`.
- **Why:** To improve DSL ergonomics. Users can choose Unix-style prefix (`repeat 3 forward`) or object-like postfix (`forward 3`).
- **Why (cont.):** Since parsing is isolated, mapping diverse syntaxes to the identical `Repeat Int Action` node adds zero burden to the interpreter!
- **Demo:** Postfix elegantly resolves to our standard AST:
  ```hs
  λ > parseInput "forward 3"
  Just (Repeat 3 Forward)
  λ > parseInput "(forward then left) 3"
  Just (Repeat 3 (Sequence [Forward, TurnLeft]))
  ```

### 6.5 Interpreting the AST: The Accumulator Pattern

We implemented the loop interpretation logic for the `Repeat` node using a classic Haskell idiom.

- **What:** Added `handleRepeat :: Int -> Action -> Game String` using the Accumulator Pattern with a local recursive `go` function.
- **Why:** To recursively execute actions `n` times while safely aggregating output messages.
- **Why (cont.):** The local `go` function provides a clean loop state. It perfectly respects our **Short-Circuit Evaluation**, halting instantly if the goal is reached.
- **Demo:** Notice the recursive accumulation logic:
  ```hs
  λ > :{
  λ | handleRepeat n action = go n ""
  λ | where
  λ |    go 0 message = pure message
  λ |    go k message = do
  λ |      newMessage <- handleAction action
  λ |      -- Evaluates state and accumulates 'newMessage'
  λ | :}
  ```

> [!TIP]
> **Architectural Reflection: The Power of Composability**\
> Notice how we created many small, focused "unit" functions throughout this journey.\
> Because they do one thing well, they are inherently easy to isolate and unit test.
>
> This makes adding new features incredibly easy.\
> We simply compose these existing foundational units together into complex behaviors without having to rewrite our core logic!

## 7. Phase 4: Programmability & Symbol Table ([MazeV9][maze-v9])

### 7.1 The Symbol Table: Macros & Aliases

To make our DSL truly programmable, we introduced the concept of variables/macros. This requires expanding our Interpreter to manage an **Environment** (Symbol Table).

- **What:** Introduced `type Alias = String` and an association list `type AliasEnv = [(Alias, Action)]`. We injected this `aliases :: AliasEnv` into our core `GameState`.
- **Why:** To support dynamic command binding (`Assign`) and execution (`Use`). We chose a simple Association List over importing a heavy `Data.Map` library. This adheres to the KISS (Keep It Simple, Stupid) principle, keeping our architecture low-entropy and dependency-free while satisfying our current scale requirements.
- **What:** Expanded the `Action` AST with `Assign Alias Action` and `Use Alias`, along with their standalone parsers (`parseAssign` and `parseUse`).
- **Demo:** Notice how the parsers elegantly capture the assignment and utilization intent:
  ```hs
  -- Parsing an Assignment (Macro definition)
  λ > runParser parseAssign "jump = forward 2"
  Just (Assign "jump" (Repeat 2 Forward),"")

  -- Parsing a Utilization (Macro execution)
  λ > runParser parseUse "use jump"
  Just (Use "jump","")
  ```

> [!NOTE]
> *These parsers are currently standalone.\
> The primary `parseAction` and the Interpreter (`handleAction`) do not yet know how to evaluate them.\
> This is our next target!*

### 7.2 Interpreting Macros & Semantic Validation (The "How")

We wired the parsers into the Interpreter by implementing `handleAssign` and `handleUse`.

- **What:** Upgraded `handleAction` to evaluate `Assign` and `Use`. Utilized State Monad's `modify` to write to the `AliasEnv` and `gets` to read from it.
- **Why:** To make the language stateful. `Assign` mutates the environment (saving the macro), while `Use` retrieves the AST and recursively feeds it back into `handleAction` for execution.
- **What:** Introduced **Semantic Validation** via `isReservedWord` and `containsUnknown`.
- **Why:** For **System Safety**. `isReservedWord` prevents users from hijacking core commands (e.g., redefining `forward`), avoiding keyword shadowing that could soft-brick the game. `containsUnknown` ensures that malformed AST nodes are strictly blocked from being saved into the Symbol Table, preventing corrupted states and runtime failures.
- **Demo:** The DSL is now fully functional and structurally safe:
  ```console
  # 1. Safe Assignment & Execution
  ➜ jump = forward 2
  Alias: "jump" defined.

  ➜ use jump
  You moved forward...

  # 2. Semantic Validation in Action
  ➜ forward = turn left
  error: "forward" is reserved and cannot be redefined.
  ```

> [!WARNING]
> **Architectural Tech Debt Reached!**\
> When we try to define a complex macro that nests other macros (e.g., `backjump = (use backward) then (use jump)`), our ad-hoc parser fails and yields an `Unknown` command.\
> While our AST and Interpreter are solid, our Parser has reached its breaking point.\
> It's time to stop patching and do a full **Parser Revamp**.

## 8. Phase 5: The Parser Revamp & Formal Grammar ([MazeV10][maze-v10])

### 8.1 The Parser Revamp: Top-Down Design

### Architectural Philosophy: Bottom-Up Discovery, Top-Down Execution

As we added features iteratively up to [MazeV9][maze-v9], our parser grew organically (bottom-up) until it became a tangled, ad-hoc mess.\
We piled combinators on top of each other until the entropy became unmanageable, making it fragile to modify.

> *"Everything should be built **top-down**, except the first time."*\
> — Alan Perlis

Another related quote:
> *"You cannot teach beginners **top-down** programming, because they don't know which end is up"*\
> — Alan Perlis

We built it bottom-up the first time to discover our domain requirements.\
Now that we understand the full scope of our DSL, it is time to step back and re-architect it **top-down** using Formal Grammar principles.

We refactored the entire parser from the top-down, modeling it after a formal language grammar.\
This resolves the ambiguity and fragility of the previous ad-hoc approach.

- **What:** Re-architected the parser into a **Recursive Descent** structure based on these grammar rules:
    - **`action ::= statement | expression`**: The top-level input is either a statement (like an assignment) or an expression (something that evaluates to an action).
    - **`statement ::= name "=" expression`**: A statement is defined as a name, an equals sign, and any valid expression.
    - **`expression ::= term ("then" term)*`**: An expression is one or more `term`s chained together by `then`.
- **Why:** This formal structure eliminates ambiguity. It creates a clear hierarchy where complex structures (`expression`) are built from simpler ones (`term`), preventing the infinite recursion and parsing conflicts we faced in `MazeV9`. It makes the parser predictable and easy to extend.
- **Demo:** The new implementation directly mirrors the grammar rules, making the code self-documenting.
  ```hs
  -- Top level: action ::= statement | expression
  parseAction :: Parser Action
  parseAction =
        parseAssign
    <|> parseExpression
  
  -- Statement: name ::= expression
  parseAssign :: Parser Action
  parseAssign = do
    name <- some (satisfy isAlphaNum)
    word "="
    body <- parseExpression -- Crucially, the body is now ANY valid expression
    pure $ Assign name body
  
  -- Expression: term ("then" term)*
  parseExpression :: Parser Action
  parseExpression = do
    first <- parseTerm
    rest  <- many (word "then" *> parseTerm)
    pure $ case rest of
      [] -> first
      xs -> Sequence (first : xs)
  ```

### 8.2 Handling Precedence & Ambiguity: The Term Layer

Next, we descended one level deeper from `expression` into `term`.

- **What:** Implemented `parseTerm` to elegantly resolve Prefix (`repeat n action`) and Postfix/Plain (`action n?`) parsing.
- **Why:** To mathematically isolate syntax ambiguity. By pushing the prefix/postfix evaluation down to the `term` layer, our `parseExpression` (the layer above) remains blissfully ignorant of these details. It only cares about chaining `term`s together. This demonstrates how Formal Grammar naturally manages **Operator Precedence** and separates concerns.
- **Demo:** The implementation is strikingly simple and maps directly to our grammar rules:
  ```hs
  -- term ::= "repeat" n atom/parentheses
  --        | atom/parentheses n?
  parseTerm :: Parser Action
  parseTerm =
        parseRepeatPrefix
    <|> parsePostfixOrPlain
  ```

### 8.3 The Deepest Layer: Atoms and Optional Modifiers

At the very bottom of our parser tree, we need to handle raw commands and their potential postfix modifiers (e.g., `forward` vs `forward 3`).

- **What:** Implemented `parsePostfixOrPlain` utilizing the `optional Applicative` combinator from `Control.Applicative`.
- **Why:** To eliminate redundant backtracking and parser ambiguity. In the old bottom-up approach ([MazeV9][maze-v9]), having separate `<|>` branches for postfix and plain actions created fragile evaluation paths. By parsing the base `parseAtomOrParentheses` once, and then looking for an `optional parseInt`, we guarantee a deterministic, single-pass decision path. If n exists, it elevates the action to a `Repeat` node; if not, it returns the plain action. Pure mathematical elegance.
- **Demo:** Notice how it gracefully folds both paths into a single unified logic block:
  ```hs
  parsePostfixOrPlain :: Parser Action
  parsePostfixOrPlain = do
    action <- parseAtomOrParentheses
    optN   <- optional parseInt
    pure $ case optN of
      Nothing -> action
      Just n  -> Repeat n action
  ```

### 8.4 The Terminal Nodes & Mutual Recursion

At the absolute bottom of our grammar, we define the terminal nodes (Atoms) and the mechanism to loop back to the top of the tree.

- **What:** Implemented `parseAtomOrParentheses` to handle parenthesized expressions, macro utilizations (`parseUse`), core atomic commands (`parseAtomicAction`), and safe fallbacks (`parseUnknown`). 
- **Why:** This is where **Mutual Recursion** happens safely. By allowing `parseAtomOrParentheses` to call the top-level `parseExpression` *only* when it is successfully wrapped `between "(" ")"` tokens, we guarantee that the parser consumes input before recursing. This permanently breaks the infinite loop trap while allowing infinitely nested logic (e.g., `repeat 3 (use jump then (forward 2))`).
- **Demo:** The implementation elegantly routes terminal commands or safely recurses back up the syntax tree:
  ```hs
  parseAtomOrParentheses :: Parser Action
  parseAtomOrParentheses = do
    between (word "(") (word ")") parseExpression
    <|> parseUse
    <|> parseAtomicAction
    <|> parseUnknown
  ```

> [!TIP]
> **The Ultimate Benefit of Top-Down Grammar**\
> Because we re-designed the parsers structurally from the top (`parseAction` -> `parseExpression` -> `parseTerm` -> `parseAtom`), we **permanently eliminated the risk of Infinite Recursion** that plagued our ad-hoc bottom-up parsers.\
> Consequently, we updated our root execution in `parseInput` from `runParser parseSequence` to `runParser parseAction`, officially transitioning our DSL to this new, robust engine.

### 8.5 The Final Wiring: Activating the New Engine

With all terminal nodes (`parseUse`, `parseAtomicAction`, `parseUnknown`) implemented, the Top-Down parser tree is fully connected.\
The final step was to wire this new engine into the game's entry point.

- **What:** Replaced `parseSequence` with `parseAction` inside the main `parseInput` execution block.
- **Why:** To officially switch the game's DSL engine from the old, fragile bottom-up parser to the new, robust top-down parser. Because our new `parseAction` natively handles expressions, statements, and sequences through its grammar rules, it serves as the perfect universal entry point.
  ```diff
    parseInput :: String -> Maybe Action
    parseInput input = case run input of
      Just (a, _) -> Just a
      Nothing     -> Nothing
      where
  -     run = runParser parseSequence  <-- Old Bottom-Up Engine
  +     run = runParser parseAction       <-- New Top-Down Engine
  ```

### Architecture Achieved: The Formal DSL

By completing this parser revamp, we have successfully transformed an ad-hoc collection of string matching functions into a **Formal Domain-Specific Language**.\
Our parser now behaves exactly like a real compiler's front-end: it strictly follows formal grammar rules, isolates syntax ambiguities at the lowest levels, and safely allows infinite composability without risking stack overflows or infinite recursions.

**The Ultimate Demo: Nested Macro Expansion**

To prove the power of our new engine, let's look back at the exact command that broke our old ad-hoc parser in `MazeV9`.\
Because our AST now natively supports infinite composability via Mutual Recursion, we can safely define and execute deeply nested macros:

```console
# 1. Define foundational macros
➜ backward = left then left
Alias: "backward" defined.

# 2. Define a complex macro that inherently re-uses other macros!
➜ backjump = (use backward) then (use jump) then (use backward)
Alias: "backjump" defined.

# 3. The state before execution
➜ map
...
[S][_][>][_][?][?][?][?]
...

# 4. Execute the nested macro tree
➜ use backjump
You turned left.
... (Interpreter recursively unfolds the AST and applies state mutations) ...
You turned left.

-- 5. The state after execution
➜ map
...
[>][_][_][_][?][?][?][?]
...
```

[maze-v5]: ../day-7/MazeV5.hs
[maze-v6]: ./MazeV6.hs
[maze-v7]: ./MazeV7.hs
[maze-v8]: ./MazeV8.hs
[maze-v9]: ./MazeV9.hs
[maze-v10]: ./MazeV10.hs
[parser-v2]: ./ParserV2.hs
[parser-v3]: ./ParserV3.hs
[parser-v4]: ./ParserV4.hs
[parser-v5]: ./ParserV5.hs
