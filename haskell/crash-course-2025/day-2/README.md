# Day 2

## FizzBuzz with infinite lists

We will make FizzBuzz in Haskell using functional programming.\
For numbers from 1 to infinity:
- Print "Fizz" when the number can be divided by 3
- Print "Buzz" when the number can be divided by 5
- Print "FizzBuzz" when the number can be divided by both 3 and 5

> [!warning]
> This is not the fastest way, but it shows another way to think in Functional Programming.

### Building the Solution

We build the answer step by step from smaller parts.

**Step 1: Make the pattern for Fizz**

```
Fizz     = _  _  F  _  _  F  _  _  F  _  _  F  _  _  F  ...
```
**Every 3rd position** shows "F" (for Fizz), other positions are empty.

**Step 2: Make the pattern for Buzz**

```
Buzz     = _  _  _  _  B  _  _  _  _  B  _  _  _  _  B  ...
```
**Every 5th position** shows "B" (for Buzz), other positions are empty.

**Step 3: Put Fizz and Buzz together**
```
FzBz     = _  _  F  _  B  F  _  _  F  B  _  F  _  _ FB  ...
```
When we combine them, position 15 shows "FB" (FizzBuzz) because it's **both 3rd and 5th**.

**Step 4: Mix with numbers to get the final result**
```
ns       = 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15  ...
         = 1  2  F  4  B  F  7  8  F  B 11  F 13 14 FB  ...
```
Replace empty positions with actual numbers.

### Implementation

#### Creating infinite lists

We will create infinite lists for Fizz and Buzz using `cycle`.

```hs
λ> fizz = cycle ["", "", "Fizz"]

λ> buzz = cycle ["", "", "", "", "Buzz"]
```

#### Combining Fizz and Buzz

First, let's zip (connect) them together:

```hs
λ> zippedFizzBuzz = zip (take 30 fizz) (take 30 buzz)
```

Then we need a function to add pairs together:

```hs
λ> :{
λ| addPair :: (String, String) -> String
λ| addPair (a, b) = a ++ b
λ> :}

λ> map addPair zipped
["","","Fizz","","Buzz","Fizz","","","Fizz","Buzz","","Fizz","","","FizzBuzz","","","Fizz","","Buzz","Fizz","","","Fizz","Buzz","","Fizz","","","FizzBuzz"]
```

**Simpler way, using `zipWith`**:

```hs
λ> fzbz = zipWith (++) fizz buzz
λ> take 30 fzbz
["","","Fizz","","Buzz","Fizz","","","Fizz","Buzz","","Fizz","","","FizzBuzz","","","Fizz","","Buzz","Fizz","","","Fizz","Buzz","","Fizz","","","FizzBuzz"]
```

## Concepts Used

### The `cycle` Function

Create an infinite list by repeating a pattern forever.

```hs
λ> cycle [1,2,3]
[1,2,3,1,2,3,1,2,3,...]
```

### The `zip` Function

Think about zipping two lists together like a zipper on clothes.\
Or buttoning two lists together like shirts.

```hs
λ> zip [1..10] ['a'..'j']
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i'),(10,'j')]
```

### The `zipWith` Function

`zipWith` is like `zip`, but it combines the elements using a function you provide.

```hs
λ> zipWith (+) [1,2,3] [4,5,6]
[5,7,9]

λ> zipWith (++) ["Hello ", "Good "] ["World", "Day"]
["Hello World","Good Day"]
```

### List Ranges

Create lists with a range of values.

```hs
λ> [1..10]
[1,2,3,4,5,6,7,8,9,10]

λ> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"

λ> [1..]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,... -- infinite list
```

With ranges, we can also specify steps:

```hs
λ> [1,3..20]
[1,3,5,7,9,11,13,15,17,19]

λ> [20,18..1]
[20,18,16,14,12,10,8,6,4,2]

-- For infinite list with steps:
λ> [2,4..]
[2,4,6,8,10,12,14,16,18,...
```

But infinite, in Haskell, lists will not be evaluated until needed:

```hs
λ> ns = [1..]

λ> ns
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,... -- infinite list

-- We can take the first 10 elements like this:
```hs
λ> take 10 ns
[1,2,3,4,5,6,7,8,9,10]
```

### List Comprehensions

A way to make new lists by describing what we want.

Syntax:

```hs
[ expression | element <- list, condition ]
```

It is read as: "Make a list of `expression` for each `element` which is a member of (`<-`/`∈`) `list` (set), where `condition` is true."

Basic example:

```hs
-- x is member of the set 1-10 where x is even, and we double it (see <1> below)
λ> [ x * 2 | x <- [1..10], even x ]
[4,8,12,16,20]

-- x is member of the set 1-100 where x squared is less than 50 (see <2> below)
λ> [ x | x <- [1..100], x * x < 50 ]
[1,1,2,3,4,5,6,7]

-- x is member of the set 1-10 where x squared is less than 50, and we double it (see <3> below)
λ> [x * 2 | x <- [1..10], x * x < 50]
[2,4,6,8,10,12,14]

-- w is member of the words in the given string where length of w is at least 3 (see <4> below)
```hs
λ> [w | w <- words "this is a cat this is a bat that is a rat", length w >= 3]
["this","cat","this","bat","that","rat"]
```

1) The *set* of `X` that `x` is member of `[1..10]` where x is even, and we double it can be expressed mathematically as:
  $$X = \{ 2x \mid x \in \mathbb{N}, 1 \leq x \leq 10, x \mod 2 = 0 \}$$

2) The *set* of `X` that x is member of [1..100] where x squared is less than 50 can be expressed mathematically as:
  $$X = \{ x \mid x \in \mathbb{N}, 1 \leq x \leq 100, x^2 < 50 \}$$

3) The *set* of `X` that x is member of [1..10] where x squared is less than 50, and we double it can be expressed mathematically as:
  $$X = \{ 2x \mid x \in \mathbb{N}, 1 \leq x \leq 10, x^2 < 50 \}$$

4) The *list* of `W` where `w` is a member of the words in the given string and the length of `w` is at least `3` can be expressed mathematically as:
  $$
  \begin{align}
  W &= \langle w \mid w \in \text{Words}(\text{"this is a cat this is a bat that is a rat"}), |w| \geq 3 \rangle \\
    &= \langle w \mid w \in \langle\text{``this"}, \text{``is"}, \text{``a"}, \text{``cat"}, \text{``this"}, \text{``is"}, \text{``a"}, \text{``bat"}, \text{``that"}, \text{``is"}, \text{``a"},  \text{``rat"}, \rangle|w| \geq 3 \rangle
    &= \langle \text{``this"}, \text{``cat"}, \text{``this"}, \text{``bat"}, \text{``that"}, \text{``rat"} \rangle \\
  \end{align}
  $$

## Related and Another Concepts

### Generator

The syntax `[1..10]` creates a list from 1 to 10, and `['a'..'z']` creates a list from 'a' to 'z'.

```hs
λ> [1..10]
[1,2,3,4,5,6,7,8,9,10]

λ> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
```

Behind the scenes, it is implemented by `succ` (Successor) function to get the next item.

```hs
λ> succ 1
2

λ> succ 9999
10000

λ> succ 'a'
'b'

λ> succ 'z'
'{'
```

### Infix Functions

In Haskell, every operator is actually a function.\
For example, `(+)` is a function that adds two numbers.

```hs
λ> 2 + 3
5
```

We can also use it in prefix form by enclosing it in parentheses:
```hs
λ> (+) 2 3
5
```

**Why is this useful?**
Because we can make them read more naturally by defining our own infix functions.

```hs
λ> import Data.List (delete)

λ> :type delete
delete :: Eq a => a -> [a] -> [a]

λ> delete 3 [1..5]
[1,2,4,5]
```

We can use it as an infix operator by surrounding it with backticks:

```hs
λ> 3 `delete` [1..5]
[1,2,4,5]
```

**Making our own infix functions:**

```hs
λ> import Data.Function (flip)

λ> :type flip
flip :: (a -> b -> c) -> b -> a -> c

λ> :type flip delete
flip delete :: Eq a => [a] -> a -> [a]

λ> without = flip delete

λ> [1..5] `without` 3
[1,2,4,5]
```

> [!note]
> "List of 1 to 5, without 3!"\
> See how naturally it reads?

### The `map` Function

How to create a `map` function that applies a function to each element of a list?

```hs
-- From Pattern Matching
λ> map' :: (a -> b) -> [a] -> [b]
λ> map' _ []     = []
λ> map' f (x:xs) = f x : map'' f xs

-- From List Comprehensions
λ> map'' :: (a -> b) -> [a] -> [b]
λ> map'' f xs = [ f x | x <- xs ]
```

Usage:
```hs
λ> :{
λ| sqr :: Int -> Int -> Int
λ| sqr x = x * x
λ> :}

λ> map' sqr [1..10]
[1,4,9,16,25,36,49,64,81,100]

λ> map'' sqr [1..10]
[1,4,9,16,25,36,49,64,81,100]

-- Using built-in map
```hs
λ> import Data.List (map)
λ> map sqr [1..10]
[1,4,9,16,25,36,49,64,81,100]
```

### The `it` Variable

In GHCi, we have useful variable `it` that holds the result of the last evaluated expression.
It allows us to do trial and error.

In GHCi, we have a special variable called `it`.\
This variable saves the result of the last thing you typed.

It helps us able to trial and error step-by-step.

```hs
λ> "this cat this bat this rat"
"this cat this bat this rat"
λ> it
"this cat this bat this rat"

λ> words it
["this","cat","this","bat","this","rat"]

λ> nub it
["this","cat","bat","rat"]

λ> length it
4

-- First we use `words `it`, then `nub it`, then `length it`... This means `length` after `nub` after `words`
-- Now we know how to put these functions together: `length . nub . words`
λ> (length . nub . words) "this cat this rat this bat"
4

λ> countUniqueWords = length . nub . words
λ> countUniqueWords "this cat this rat this bat"
```

### Higher-Order functions

A high-order function is a function that can:

- Take other functions as input
- Return a new function as output

Let's look at `zipWith` to understand this better.

#### Using `zipWith`

We can use `zipWith` to combine two lists using a function:

```hs
λ> zipWith (+) [1..10] [10,9..1]
[11,11,11,11,11,11,11,11,11,11]
```

#### Understanding the Type

```hs
λ> :type zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
```

> [!note]
> Breaking down the type of `zipWith`:
>
> - `a -> b -> c` means: a function that takes `a` and `b`, returns `c`
> - `[a] -> [b] -> [c]` means: takes list of `a`, then list of `b`, returns list of `c`
> - Function arrows `->` are right-associative: `a -> b -> c` = `a -> (b -> c)`

#### How zipWith Works with `+`

First, let's see the type of `+`:

```hs
λ> :type (+)
(+) :: Num a => a -> a -> a
```

When we give `+` to `zipWith`:

```hs
λ> :type zipWith (+)
zipWith (+) :: Num c => [c] -> [c] -> [c]
```

This gives us a NEW function that adds two lists!

#### Creating New Functions

We can use the result of `zipWith (+)` directly:

```hs
λ> zipWith (+) [1..] [10,9..1]
[11,11,11,11,11,11,11,11,11,11]
```

Or with parentheses (same thing):

```hs
λ> (zipWith (+)) [1..] [10,9..1]
[11,11,11,11,11,11,11,11,11,11]
```

#### Naming Our New Function

We can give this new function a name:

```hs
λ> zipAndAdd = zipWith (+)

λ> zipAndAdd [1..] [10,9..1]
[11,11,11,11,11,11,11,11,11,11]
```

Now `zipAndAdd` is a function that adds two lists element by element!

#### Why This Matters

This shows how high-order functions let us:

- **Create new functions** from existing ones
- **Reuse functions** in different ways
- **Write less code** by combining functions
