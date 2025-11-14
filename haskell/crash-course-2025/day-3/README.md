# Day 3

## Generic Programming

In Haskell, many functions work with any type.\
This is called generic programming.

One example is the list operator `(:)` which adds an element to the front of a list.

```hs
λ> :type (:)
(:) :: a -> [a] -> [a]

λ> :type (:) 'a'
(:) 'a' :: [Char] -> [Char]
```

When we use `(:)` with a character like `'a'`, we get a function.\
This function takes a list of characters and returns a new list.

```hs
λ> addD = (:) 'D'

λ> :type addD
addD :: [Char] -> [Char]

λ> addD "ave"
"Dave"
```

Why is Haskell so powerful?\
Because it has very good type inference and generic programming!

Now let's see why we cannot compare a number and a string using `(==)`.

Its type!

```hs
λ> :type (==)
(==) :: Eq a => a -> a -> Bool
```

We cannot use `(==)` to compare different types.\
Both values must be the same type `a`.\
Also, type `a` must be an instance of `Eq`.

```hs
λ> :t 'x'
'x' :: Char

λ> :t (==) 'x'
(==) 'x' :: Char -> Bool

λ> (==) 'x' 9
<interactive>:38:10: error: [GHC-39999]
    • No instance for 'Num Char' arising from the literal '9'
    • In the second argument of '(==)', namely '9'
      In the expression: (==) 'x' 9
      In an equation for 'it': it = (==) 'x' 9
```

## Currying

### Why currying?

There is a mathematician named Haskell Curry who used currying very often in his work.\
The Haskell programming language is named after him to honor his work.

In Haskell, all functions take only one argument.\
If we want to pass multiple arguments, we can do it by returning a new function for each argument.

### Currying in action

If we have a function:
$$ f(x, y) = x + y $$

Then:
- **Curried:** $g = \text{curry}(f) = \lambda x. \lambda y. x + y$
- **Uncurried:** $h = \text{uncurry}(g) = \lambda (x, y). x + y$

Haskell has built-in functions for this:

```hs
λ> :type curry
curry :: ((a, b) -> c) -> a -> b -> c

λ> :type uncurry
uncurry :: (a -> b -> c) -> (a, b) -> c
```

### Uncurry

`(a -> b -> c) -> (a, b) -> c`

Take a function that takes `a` and returns a function that takes `b` and returns `c`,\
and return a new function that takes a tuple `(a, b)` and returns `c`.

```hs
λ> add :: Int -> Int -> Int
λ> add x y = x + y

λ> :type uncurry add
uncurry add :: (Int, Int) -> Int

λ> addPair = uncurry add

λ> :type addPair
addPair :: (Int, Int) -> Int

λ> addPair (3, 5)
8
```

### Curry

`((a, b) -> c) -> a -> b -> c`

Take a function that takes a tuple `(a, b)` and returns `c`,\
and return a new function that takes `a`, then `b`, and returns `c`.

```hs
λ> add' = curry addPair

λ> :t add'
add' :: Int -> Int -> Int

λ> add' 3 5
8
```

## From Day 2's Homework

### Permutations

#### Recursive algorithm

A permutation is a rearrangement of a list.\
For example, permutations of `['a', 'b', 'c']` are:

```
abc
acb
bac
bca
cab
cba
```

- For `abc` and `acb`, `a` is the first element.\
  The remaining permutations are from `['b', 'c']`: `bc` and `cb`.
- For `bac` and `bca`, `b` is the first element.\
  The remaining permutations are from `['a', 'c']`: `ac` and `ca`.
- For `cab` and `cba`, `c` is the first element.\
  The remaining permutations are from `['a', 'b']`: `ab` and `ba`.

We can see the pattern: remove each element `x` from list `xs`, then add `x` to the front of each permutation of the remaining elements.

$$
\text{permute}(xs) = {x : ys \mid \forall x \in xs, \forall ys \in \text{permute}(xs \setminus {x})}
$$

We can implement the `permute` function in Haskell:

```hs
λ> import Data.List (delete)

λ> permute :: Eq a => [a] -> [[a]]
λ> permute [] = []
λ> permute xs = [ x:ys | x <- xs, ys <- permute (delete x xs) ]
```

#### Interleaving algorithm

Another way to create permutations is to use interleaving.

How does this work?\
Let's find permutations of `[1, 2, 3]` and add a new element `4`.

We can think of `[1, 2, 3, 4]` as inserting `4` at the front of each permutation of `[1, 2, 3]`.

```hs
4 : permute [1, 2, 3]
```

How can we insert `4` into all positions of `[1, 2, 3]`?\
It can be `4123`, `1423`, `1243`, or `1234`.

```
4 1   2   3
  1 4 2   3
  1   2 4 3
  1   2   3 4
```

> [!note]
> This is called the "interleaving" technique.

### Compression

#### Simple Case

Let's start with a string that has only one repeated character:

```hs
λ> simpleInput = "AAAAAAAAA"
```

We want the output to be `"A9"`.

How can we get just one 'A'?\
We can use `head` to get the first character!

```hs
λ> head simpleInput
'A'
```

How can we get the count of 'A'?\
We can use length to count all characters!

```hs
λ> length simpleInput
9
```

Now we have 'A' and `9`.\
But `'A'` and `9` are different types, so we can't put them together directly.

How can we combine them?\
We can use `show` to make the number into a string!

```hs
λ> :type show
show :: Show a => a -> String

λ> show 9
"9"
```

Now we can combine them with `(:)` because 'A' is a `Char` and `"9"` is `[Char]` (or `String`).

```hs
λ> compressUnit str = head str : show (length str)

λ> compressUnit simpleInput
"A9"
```

#### Real case

Now let's work with the real input:

```hs
λ> input = "AAAAAAABBBBBXXXXXBBBBBBBAAAAAA"
```

How can we split this into groups of repeated characters?\
We can use group from `Data.List`!

```hs
λ> import Data.List (group)

λ> :t group
group :: Eq a => [a] -> [[a]]

λ> group input
["AAAAAAA","BBBBB","XXXXX","BBBBBBB","AAAAAA"]
```

Now we can map `compressUnit` to each group and concatenate the results.

```hs
λ> compress str = concat $ map compressUnit (group str)

-- OR using concatMap
λ> compress' str = concatMap compressUnit (group str)

-- OR using function composition
λ> :{
λ| compress'' = concat . compressAll . group
λ|   where
λ|     compressAll = map compressUnit
λ| :}
```

Now, let's test it!

```hs
λ> compress input
"A7B5X5B7A6"

λ> compress' input
"A7B5X5B7A6"

λ> compress'' input
"A7B5X5B7A6"
```
