# Day 3 - Generic Types and Currying

Today I learned two big ideas: **Generic Types** and **Currying**.\
Then looking at some solutions for Day 2's exercises.

## Generic Programming (Working with Any Type)

In Haskell, many functions are **generic**.\
This means they can work with *any* type.

One of the great examples is the `(:)` function.\
It adds one item to the front of a list.

```hs
λ> :type (:)
(:) :: a -> [a] -> [a]
```

The type `a` is a **placeholder**.\
It means "any type".\
So, `(:)` takes:

1. A value of "any type" `a`
2. A list of that _same type_ `[a]`
3. ...and gives back a new list of `[a]`.

If we give it a `Char` (a character), `a` becomes `Char`:

```hs
λ> :type (:) 'a'
(:) 'a' :: [Char] -> [Char]
```

This is a new function!\
It's waiting for a list of `Char`s.\
We can even give it a name:

```hs
λ> addD = (:) 'D'

λ> :type addD
addD :: [Char] -> [Char]

λ> addD "ave"
"Dave"
```

### Type Constraints (Rules for Types)

Why can't we compare a number and a string?\
Let's look at the type for `(==)` (equals).

```hs
λ> :type (==)
(==) :: Eq a => a -> a -> Bool
```

This looks similar, but has a new part: `Eq a =>`

- `a -> a -> Bool`: This means it takes two values of the *same type* `a` and returns a `Bool` (`True` or `False`).
- `Eq a =>`: This is a **constraint** (a rule).\
  It means "`a` can be any type, *as long as* it knows how to be equal."\
  The type `a` must be part of the `Eq` (short for 'Equality') type class.

**Numbers** know how to be equal.\
**Strings** know how to be equal.\
But a `Char` and a `Num` (number) are not the same type `a`.

```hs
λ> 'x' == 9
<interactive>:38:10: error:
    • No instance for (Num Char) arising from the literal ‘9’
    ...

λ> :t (==) 'x'
(==) 'x' :: Char -> Bool
```

Haskell gets confused.\
It says: "I don't know how to make a 'Num' (number) out of a 'Char' (character). They are just different types!"

---

## Currying (Functions taking one thing at a time)

### Why is it called Currying?

There was a famous mathematician named **Haskell Curry**.\
He used this idea a lot.\
The Haskell programming language is named after him to honor his work!

### What is Currying?

**The Main Rule of Currying:**

In Haskell, all functions *really* only take **one** argument.

"Wait," you say...\
"`add x y` looks like two arguments!"

Here is the secret: `add` is really a function that takes *one* argument (`x`) and *returns a new function*.\
That *new function* then takes one argument (`y`).

`add 3 5` is really two steps:

1. `(add 3)`: This runs first.\
	It returns a *new function* that "adds 3 to anything".
2. `5`: We give `5` to that new function.
3. The result is `8`.

### `curry` and `uncurry`

To see how these work, let's first think in plain math.
If we have a function:
$$ f(x, y) = x + y $$

Then:

- **Uncurried:** This is the "normal" way.\
  The uncurried function $f_\text{pair}$ is **created by the `uncurry` function**.\
  It takes an existing curried function ($f_{\text{curried}}$).\
  Then, turns it into a new function that takes a single pair (tuple) `(x, y)` all at once.
  $$f_{\text{pair}} = \text{uncurry}(f_\text{curried}) = \lambda (x, y). x + y$$
- **Curried:** This is the "Haskell" way.\
  The curried function ($f_{\text{curried}}$) is **created by the `curry` function**\
  It takes an existing *pair* function ($f_{\text{pair}}$).\
  Then, turns it into a new "curried" function.\
  This new function takes one input `x`, and returns a new function that takes one input `y`.
  $$f_{\text{curried}} = \text{curry}(f_{\text{pair}}) = \lambda x. \lambda y. x + y$$

**`curry` and `uncurry` are functions that convert between these two styles:**

- `curry` **converts** the "pair" function into the "chain" function.
  $f_{\text{curried}} = \text{curry}(f_{\text{pair}})$

- `uncurry` **converts** the "chain" function back into the "pair" function.
  $f_{\text{pair}} = \text{uncurry}(f_{\text{curried}})$

Haskell has built-in helpers to show this idea:

- `uncurry`: Takes a curried function (like `add`) and turns it into a *new function* that takes a single pair (a tuple).
- `curry`: Does the opposite. It takes a function that uses a tuple and turns it into a curried function.

#### Uncurry Example

```hs
λ> :type uncurry
uncurry :: (a -> b -> c) -> (a, b) -> c

λ> add :: Int -> Int -> Int
λ> add x y = x + y

-- Let's uncurry 'add'
λ> addPair = uncurry add

λ> :type addPair
addPair :: (Int, Int) -> Int

-- 'addPair' now takes one tuple
λ> addPair (3, 5)
8
```

#### Curry Example

```hs
λ> :type curry
curry :: ((a, b) -> c) -> a -> b -> c

-- Let's curry 'addPair'
λ> add' = curry addPair

λ> :type add'
add' :: Int -> Int -> Int

-- 'add'' is now a normal curried function
λ> add' 3 5
8
```

Why we need this?\
Because sometimes our friend gives us a response from third-party library/API in tuple form.

Assumed response from API:

```hs
λ> response :: (Int, Int)
λ> response = (10, 20)
```

We have the declared `add` function:

```hs
λ> add :: Int -> Int -> Int
λ> add x y = x + y
```

Now, we have 2 choices:

1. Manually extract values from tuple and use our curried function

```hs
λ> add (fst response) (snd response)
30
```

2. Use 'uncurry' to convert our curried function to accept tuple

```hs
λ> uncurry add response
30
```

---

## Exercise Solutions (from Day 2)

Here are some ways to solve the homework problems.

### 1. Permutations (All arrangements)

A permutation is a rearrangement of a list.\
For `['a', 'b', 'c']`, the permutations are:

```
abc
acb
bac
bca
cab
cba
```

#### Recursive Idea

We can see a pattern:

- Start with `a` → Find all permutations of `['b', 'c']` (which are `bc` and `cb`)
    - Put `a` on the front: `abc`, `acb`
- Start with `b` → Find all permutations of `['a', 'c']` (which are `ac` and `ca`)
    - Put `b` on the front: `bac`, `bca`
- Start with `c` → Find all permutations of `['a', 'b']` (which are `ab` and `ba`)
    - Put `c` on the front: `cab`, `cba`

The pattern is:

1. Pick an element `x` from the list `xs`
2. Find all permutations of the *rest* of the list (without `x`)
3. Add `x` to the front of each of those permutations

$$
\text{permute}(xs) = {x : ys \mid \forall x \in xs, \forall ys \in \text{permute}(xs \setminus {x})}
$$

Here is the code:

```hs
λ> import Data.List (delete)

λ> permute :: Eq a => [a] -> [[a]]
λ> permute [] = [[]] -- The only permutation of empty is empty
λ> permute xs = [ x:ys | x <- xs, ys <- permute (delete x xs) ]
```

**How to read this hard code:** `[ x:ys | ... ]`

- `x <- xs`: Get each item `x` from the list `xs` (e.g., 'a')
- `delete x xs`: Make a new list *without* `x` (e.g., "bc")
- `permute (...)`: Find all permutations of that smaller list (e.g., ["bc", "cb"])
- `ys <- ...`: Get each permutation `ys` from that new list (e.g., first "bc", then "cb")
- `x:ys`: Put `x` back on the front (e.g., 'a' : "bc" = "abc")

#### Interleaving Idea

Another way is to "interleave" (weave) a new item into a list.\
If we have `[1,2,3]`, how can we add `4`?\
We can put `4` in every possible spot:

```
4 1 2 3  (at the front)
1 4 2 3  (in the 2nd spot)
1 2 4 3  (in the 3rd spot)
1 2 3 4  (at the end)
```

To be clearer:

```
4 1   2   3
  1 4 2   3
  1   2 4 3
  1   2   3 4
```

This is another valid way to think about permutations.

### 2. Compression

We want to turn `"AAAAABBBCC"` into `"A5B3C2"`.

#### Simple Case: One Character

Let's start with an easy input: `"AAAAAAAAA"`.\
We want the output `"A9"`.

1. How to get the 'A'?\
  We can use `head` to get the first item.

    ```hs
    λ> head "AAAAAAAAA"
    'A'
    ```

2. How to get the count?\We can use `length` to count all items.

    ```hs
    λ> length "AAAAAAAAA"
    9
    ```

3. How to join them?\
	We have a `Char` ('A') and an `Int` (9).\
	We can't join them.\
	We must turn the number `9` into a string `"9"` using `show`.

		```hs
    How can we combine them?\
We can use `show` to make the number into a string!

    ```hs
    λ> :type show
    show :: Show a => a -> String

		λ> show it
		"9"
		```

4. Now we can use `(:)` to add the `Char` 'A' to the front of the `String` "9".

		```hs
		λ> 'A' : it
		"A9"
		```

Let's make a function for this!

```hs
λ> compressUnit str = head str : show (length str)

λ> compressUnit "AAAAAAAAA"
"A9"

λ> compressUnit "BBBB"
"B4"
```

#### Real Case: Many Characters

Now for the real input:

```hs
λ> input = "AAAAAAABBBBBXXXXXBBBBBBBAAAAAA"
```

How can we split this into groups?\
We can use `group` from `Data.List`!

```hs
λ> import Data.List (group)

λ> :t group
group :: Eq a => [a] -> [[a]]

λ> group input
["AAAAAAA","BBBBB","XXXXX","BBBBBBB","AAAAAA"]
```

This is perfect!\
Now we have a list of strings.\
We can use `map` to run our `compressUnit` function on *each* string in this list.

```hs
λ> map compressUnit (group input)
["A7","B5","X5","B7","A6"]
```

We are so close!\
This is a list of strings.
We just need to join them all into one string.\
We use `concat` (short for concatenate, or "join").

```hs
λ> concat ["A7","B5","X5","B7","A6"]
"A7B5X5B7A6"
```

Let's put it all together in one function:

```hs
λ> compress str = concat $ map compressUnit (group str)

λ> compress input
"A7B5X5B7A6"
```

**Bonus ways to write this:**

We can use `concatMap`, which does `map` and then `concat` in one step.

```hs
λ> compress' str = concatMap compressUnit (group str)
λ> compress' input
"A7B5X5B7A6"
```

Or we can use function composition `(.)` to chain functions.\
(Read this backwards: first `group`, then `map compressUnit`, then `concat`).

```hs
λ> :{
λ| compress'' = concat . compressAll . group
λ|   where
λ|     compressAll = map compressUnit
λ| :}
λ> compress'' input
"A7B5X5B7A6"
```
