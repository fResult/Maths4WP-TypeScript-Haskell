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

## Permutations

### Recursive algorithm

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

### Interleaving algorithm

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
