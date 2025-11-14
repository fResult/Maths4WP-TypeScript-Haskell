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

