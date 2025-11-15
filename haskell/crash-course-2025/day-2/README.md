# Day 2 - FizzBuzz and Core Haskell Concepts

This guide shows a fun way to make FizzBuzz in Haskell.

> [!warning]
> This is not the fastest way to make FizzBuzz.\
> It just shows another way to think in Functional Programming

## The FizzBuzz Challenge

Here is our goal.\
We want to print numbers from 1 to infinity.

- If a number divides by 3, print "Fizz".
- If a number divides by 5, print "Buzz".
- If a number divides by 3 AND 5, print "FizzBuzz".
- If none of those, just print the number.

### Building the Solution

We will build the answer in small steps.

**Step 1: Make the 'Fizz' pattern.**

We put "F" (Fizz) in every 3rd spot.

```
Fizz     = _  _  F  _  _  F  _  _  F  _  _  F  _  _  F  ...
```

**Step 2: Make the 'Buzz' pattern.**

We put "B" (Buzz) in every 5th spot.

```
Buzz     = _  _  _  _  B  _  _  _  _  B  _  _  _  _  B  ...
```

**Step 3: Put Fizz and Buzz together**

```
FzBz     = _  _  F  _  B  F  _  _  F  B  _  F  _  _ FB  ...
```
Look! Spot 15 gets "FB" (FizzBuzz).

**Step 4: Mix with numbers.**

We replace the empty spots (`_`) with the real number.

```
ns       = 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15  ...
Result   = 1  2  F  4  B  F  7  8  F  B 11  F 13 14 FB  ...
```
Replace empty positions with actual numbers.

---

## Basic Tools for the Job

### List Ranges (and Infinite Lists)

Haskell is **lazy**.\
This is a very important idea.\
It means Haskell only does work when it needs to.

If we make a list `[1..]`, it is an infinite list.\
The computer does not crash.

**Why?**\
Because Haskell waits.\
If we say `take 10 [1..]`, Haskell gets the first 10 items and then stops.\
It is very efficient.

```hs
λ> [1..10]
[1,2,3,4,5,6,7,8,9,10]

λ> [1,3..20]
[1,3,5,7,9,11,13,15,17,19]

λ> [20,18..1]
[20,18,16,14,12,10,8,6,4,2]

λ> [1..]
[1,2,3,4,5,6,7,8,9,10,11,12,... -- goes forever

λ> take 10 [1..]
[1,2,3,4,5,6,7,8,9,10]
```

### The `cycle` Function

`cycle` makes an infinite list.\
It repeats a small list forever.

```hs
λ> cycle [1,2,3]
[1,2,3,1,2,3,1,2,3,...]
```

### The `show` function

`show` is a helper function.\
It turns a number into a string.\
(A string is just text).


```hs
λ> show 10
"10"
```

### The `map` Function

`map` takes a function.\
It applies that function to *every* item in a list.

```hs
λ> map show [1,2,3]
["1","2","3"]
```

### The `zip` Function

`zip` is like a zipper on a jacket. It joins two lists together, item by item. It makes a list of pairs (called tuples).

```hs
λ> zip [1,2,3] ['a','b','c']
[(1,'a'),(2,'b'),(3,'c')]
```

### The `zipWith` Function

`zipWith` is like `zip`, but smarter. You give it a function (like `+` or `max`). It uses that function to join the two lists.

```hs
λ> zipWith (+) [1,2,3] [4,5,6]
[5,7,9]
-- (1+4=5), (2+5=7), (3+6=9)
```

---

## Assembling FizzBuzz

Now let's use our tools to build the solution.

### Step 1: Create infinite Fizz and Buzz lists

We use `cycle` to make our repeating patterns.\
`""` is just an empty string.

```hs
λ> fizz = cycle ["", "", "Fizz"]

λ> buzz = cycle ["", "", "", "", "Buzz"]
```

### Step 2: Combine Fizz and Buzz patterns

We use `zipWith` to join `fizz` and `buzz`.\
The function `(++)` joins two strings.

```hs
λ> fzbz = zipWith (++) fizz buzz

λ> take 15 fzbz
["","","Fizz","","Buzz","Fizz","","","Fizz","Buzz","","Fizz","","","FizzBuzz"]
```

### Step 3: Create the infinite list of numbers (as strings)

We need a list of all numbers, but as strings.\
We use `map` and `show` on our infinite list `[1..]`.

```hs
λ> ns = map show [1..]

λ> take 15 ns
["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"]
```

### Step 4: Combine patterns and numbers for the final result

This is the magic step.\
We use `zipWith` one last time with the `max` function.

```hs
λ> fizzBuzzResult = zipWith max fzbz ns

λ> take 15 fizzBuzzResult
["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz"]
```

> [!tip]
> **Why does `max` work?**\
> `max` picks the "bigger" of two strings.\
> Strings are sorted by alphabet.
>
> - `max "" "1"` -> returns `"1"` (The number string is "bigger" than an empty string)
> - `max "Fizz" "3"` -> returns `"Fizz"` ('F' comes *after* '3' in the alphabet, so "Fizz" is "bigger")
> - `max "FizzBuzz" "15"` -> returns `"FizzBuzz"` ('F' comes *after* '1')
>
> This automatically picks the right word or number for us!

---

## The Theory (Why this is so cool)

Why can we give functions like `(+)` or `max` to *other* functions?\
This is because of two big ideas in Haskell.

### Higher-Order Functions (HOFs)

A **Higher-Order Function** (HOF) is a function that can:

1. Take another function as an input.
2. Return a function as an output.

`map` and `zipWith` are HOFs.

- We give `map` the `show` function.
- We give `zipWith` the `(++)` or `max` function.

This lets us build small tools (like `max`) and use them in bigger tools (like `zipWith`).

#### Understanding the Type

The "type" of `zipWith` looks scary, but it is simple.\
`zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`

Let's read this:

- `(a -> b -> c)`: The first input is a function. This function must take two things (`a` and `b`) and give one thing back (`c`).
- `[a]`: The second input is a list of type `a`.
- `[b]`: The third input is a list of type `b`.
- `[c]`: The final output is a list of type `c`.

When we give `zipWith` the `(+)` function, we "fill in" the first part.\
Haskell gives us back a *new* function.\
`zipWith (+)` is a new function that is still waiting for two lists.

```hs
λ> zipAndAdd = zipWith (+)
λ> zipAndAdd [1,2,3] [10,20,30]
[11,22,33]
```

### Currying

This idea has a name: **Currying**.\
It is named after a smart man, Haskell Curry.\
(Yes, like the language!)

**The Main Rule of Currying:** In Haskell, all functions *really* only take one input.

"Wait," you say. "`add x y` looks like two inputs!"

Here is the secret: `add` takes one input, `x`.\
It then *returns a new function*.\
That new function then takes one input, `y`.

`add 3 5` is really two steps:

1. `(add 3)`: This runs first.\
  It returns a *new function* that "adds 3 to anything".
2. `5`: We give `5` to that new function.
3. The result is `8`.

This is why `zipWith (+)` works.\
We give `zipWith` just *one* input: the `(+)` function.\
`zipWith` returns a new function (`zipAndAdd`) that is waiting for the two lists.\
This idea is extremely powerful.

---

## Other Cool Haskell Ideas

Here are other useful ideas from the class.
### List Comprehensions

This is a different way to make new lists.\
It looks like math.

```
[ output | input <- list, condition ]
```

**Example:** `[ x * 2 | x <- [1..10], even x ]`

**How to read this:**

- "Get `x * 2`..."
- "...where `x` comes from the list `[1..10]`..."
- "...and only if `even x` is true."

The result is `[4,8,12,16,20]`.

---

### Infix Functions

An operator like `+` is just a function.\
`2 + 3` is the "infix" way (in the middle).\
`(+) 2 3` is the "prefix" way (at the front).\
They do the same thing.

You can make *any* function infix by using backticks (\`):\
(The key near the `1` on our keyboard).

```hs
λ> delete 3 [1,2,3,4,5]
[1,2,4,5]

-- Same command, but "infix":
λ> 3 `delete` [1,2,3,4,5]
[1,2,4,5]
```

Sometimes, this makes your code easier to read.

**More example:**

`flip` is a function that *flips* the order of inputs for another function.\
Then, we can use it to make `delete` infix more easily.

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

Very easier to read, right?

---

## Appendix

### Generator (`succ`)

The syntax `[1..10]` (from [List Ranges and Infinite Lists](#list-ranges-and-infinite-lists) section) creates a list from 1 to 10.\
Behind the scenes, it is implemented by `succ` (Successor) function to get the next item.

```hs
λ> succ 1
2

λ> succ 9999
10000

λ> succ 'a'
'b'
```

### The `it` Variable

GHCi (the Haskell program) has a helper variable: `it`.\
`it` always holds the result of the last thing you did.\
This is great for testing step-by-step.

```hs
λ> "this is a test sentence"
"this is a test sentence"

λ> it
"this is a test sentence"

λ> words it
["this","is","a","test","sentence"]

λ> length it
5
```

Then, we can compose these functions together:

```hs
λ> (length . nub . words) "this cat this rat this bat"
4

λ> countUniqueWords = length . nub . words
λ> countUniqueWords "this cat this rat this bat"
4
```
