# Day 4

## Is Prime Number?

A prime number is a number greater than 1.\
It can only be divided by 1 and itself.\
(For example: 2, 3, 5, 7, 11).

### A Simple Function: `factors`

To check if a number is prime, we first need a function to find its "factors".\
(Factors are numbers that can divide `n` perfectly).

Here is our **first attempt** (our "Make it Work" version):

```hs
λ> factors n = [ i | i <- [1..n], n `mod` i == 0 ]
```

**How to read this code:**

- `[ i | ... ]`: "Get me a list of `i`..."
- `i <- [1..n]`: "...where `i` is a number from 1 up to `n`..."
- ``n `mod` i == 0``: "...and` n`can be divided by`i`."

This function *works*.\
For example, `factors 10` gives `[1, 2, 5, 10]`.\
But is this simple function "Right"?
This brings us to a very important idea.

### A Quote for Today

> [!tip]
> "Make it work, then make it right, then make it fast."\
> — Kent Beck

This quote teaches us the right order to do things.

$$Work \to Right \to Fast$$

#### 1. Make it Work

This is the first step.\
Write a function that gives an answer.\
This is our `factors` function above.\
It is our first, simple, straightforward version.\
It *works* (it runs and gives a result).

#### 2. Make it Right

This is the second, most important step.\
We must check if our "Work" version is **logically correct**.

What does "Right" really mean?

It means our code must match the "ideal" mathematical goal of a function, $f: A \to B$.

Our goal is to make our code work for all possible inputs in set $A$.

The math ideal is:

$$(\forall a \in A) \to \exists f(a) \in B$$

This means: "For **every** input `a` in the set of all inputs $A$, our function `f` **must** give a correct result `f(a)` that is in the set of outputs $B$."

Now, let's apply this to our `isPrime` problem.

What is the 100% correct definition (our ideal)?

`isPrime n` is `True` if and only if its factors are `[1, n]`.

Let's write that logic:

```hs
isPrime n = factors n == [1, n]
```

Now, we check: Does our "Work" function (factors) make this logic correct for all numbers in set $A$?\
**Yes!** Our simple factors function perfectly matches this mathematical definition.

We must always try (drive for) this "for all" ($\forall$) goal.

We can't always guarantee 100% coverage for every problem (especially complex problems, like the "chiba" search example, which are probabilistic).\
The only way to be 100% sure is if we can prove our algorithm.

But in this lucky, simple case, our logic is provable.

So, our "Work" version is already "Right".

#### 3. Make it Fast

This is the *last* step. Our code is now **"Right"** (correct logic). But is it **"Fast"**?\
We must *only* try to make it faster *after* we know it is correct.

### Come back to Factors

Let's test if our "Right" function is "Fast".\
We can use GHCi's built-in profiler.

```hs
λ> :set +s
```

Let's test `factors` with huge numbers.\
(We use `length` to avoid I/O processing.)

```hs
λ> m1     = 1_000_000  -- 1 million
λ> m10    = m1 * 10    -- 10 million
λ> m100   = m10 * 10   -- 100 million
λ> m1000  = m100 * 10  -- 1 billion
λ> m10000 = m1000 * 10 -- 10 billion

λ> length $ factors m1
49
(0.27 secs, 200,099,120 bytes)

λ> length $ factors m10
64
(2.33 secs, 2,000,100,088 bytes)

λ> length $ factors m100
81
(23.08 secs, 20,000,101,168 bytes)

λ> length $ factors m1000
100
(233.35 secs, 200,000,103,120 bytes)
```

233 seconds!\
That's almost **4 minutes** for 1 billion.\
Our code is **"Right"**, but it is **"Slow"**.\
Now we must move to Step 3: "Make it Fast".

How?\
We find a better algorithm.\
Let's look at the factors of 100: `[1, 2, 4, 5, 10, 20, 25, 50, 100]`

They come in pairs: `1*100`, `2*50`, `4*25`, `5*20`, and `10*10`.\
The pairs meet at the square root ($\sqrt{100} = 10$).\
We only need to check up to $\sqrt{n}$!!

Here is our new, "Fast" version, `factors'`:

```hs
λ> :{
λ| factors' :: Int -> [Int]
λ| factors' n = mods ++ divs
λ|   where
λ|     mods = [ i | i <- [1..(floor . sqrt . fromIntegral) n], n `mod` i == 0 ]
λ|     divs = [ d | i <- mods, let d = n `div` i, i /= d ]
λ| :}
```

Let's test this new "Fast" version:

```hs
λ> length $ factors' m1000
100
(0.03 secs, 6,433,992 bytes)
```

From **4 minutes** down to **0.03 seconds**!\
We have successfully "Made it Fast" without breaking the "Right" logic.

We can even try larger inputs now.

```hs
λ> length $ factors' (m10000 * 10000)
225
(2.31 secs, 2,000,155,952 bytes)
λ> length $ factors' (m10000 * 100000)
256
(7.17 secs, 6,324,718,984 bytes)
λ> length $ factors' (m10000 * 1000000)
289
(22.67 secs, 20,000,172,320 bytes)
```

Just 22.67s for 10 trillion!\
That's a huge improvement from 4 minutes for 1 billion.\
We have successfully made it fast!

### Our Final `isPrime` Function

Now we have a helper function, `factors'`, that is both **Right** and **Fast**.\
We can finally build our `isPrime` function and trust it.

```hs
λ> factors' 17
[1,17]
```

We can see that 17 has only two factors: 1 and 17 itself.\
Thus, 17 is a prime number.

Let's build our prime number checker:

```hs
λ> isPrime :: Int -> Bool
λ> isPrime n = factors' n == [1, n]

λ> isPrime 17
True
```
### Why is this "Stupid"? (A Final Thought)

We call this function "stupid" (or "inefficient") for one reason:

> [!important]
> **It does too much work.**

Our goal is just a `True` or `False` answer.\
But our isPrime function asks `factors'` to build a complete list of all factors first.

- To check `isPrime 100`, our code first builds the *full list*: `[1, 2, 4, 5, 10, 20, 25, 50, 100]`
- *Then*, it checks if that list equals `[1, 100]` (it doesn't)

This is wasted work!\
A *truly* smart `isPrime` function would **stop** (or "short-circuit") the moment it finds the first factor, `2`.\
It would return `False` right away and not bother finding `4, 5, 10, 20...` etc.

But for our crash course, our version is good enough.\
It clearly follows our "Work $\to$ Right $\to$ Fast" logic.
