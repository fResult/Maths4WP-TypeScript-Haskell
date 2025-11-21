# Day 4 - Type Classes, Functors, and Recursive Structures

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

---

## There is No Function without a Set

We learned before that a function maps from one set $A$ to another set $B$.

$$f: A \to B$$

Haskell gives us many built-in sets (Types) like `Bool`, `Int`, and `Char`.

But what if we want a more complex set, like `Color`, `Customer`, or `Order`?

If we say **"a Type is a Set"**, then we must be able to define new types.

Let's start with the easiest one.\
We will make our own `Boolean` type.

### Create Our Own Type

Haskell has `Bool` (with `True`/`False`).\
We will make `Boolean` (with `Yes`/`No`) for learning.

```hs
λ> data Boolean = No | Yes
```

We just created a new type (a new set).

### Inspecting Our New Type

Let's ask Haskell about our new type using `:info`.

```hs
λ> :info Boolean
type Boolean :: *
data Boolean = No | Yes
```

**What is that `*`?**

`*` is called a "Kind".\
It means this is a **concrete type**.\
A concrete type is a type that holds real values.

- `Int` is a concrete type.
- `Char` is a concrete type.
- `Boolean` is a concrete type.

Now look at a List type:

```hs
λ> :kind []
[] :: * -> *
```

This is **not** a concrete type.\
It means "I need one concrete type (`*`) to *give you* a concrete type (`*`)."\
It's like a machine waiting for parts.

```hs
λ> :kind [String]
[String] :: *
```

Now it's a concrete type (a list of strings).

`Maybe` (Optional) is the same:

```hs
λ> :kind Maybe
Maybe :: * -> *
λ> :kind Maybe Int
Maybe Int :: *
```

What about `Either`?

```hs
λ> :kind Either
Either :: * -> * -> *
```

This machine needs *two* concrete types (e.g., one for an error, one for a result) to give us *one* final concrete type.

### Our New Type Fails: The "Show" Error

Let's try to *use* our new `Boolean` type.

```hs
λ> No
<interactive>:477:1: error: [GHC-39999]
    • No instance for ‘Show Boolean’ arising from a use of ‘print’
    ...
```

It fails! Why?\
Haskell doesn't know how to **show** (print) our `Boolean` as a string.\
It says: "I have `No instance for 'Show'`."

But the compiler _does_ know the type:

```hs
λ> :type No
No :: Boolean

λ> bool = No
-- No error
```

The problem is not the type.\
The problem is **printing**.\
The `print` function needs its input to have the `Show` characteristic.

```hs
λ> :type print
print :: Show a => a -> IO ()
```

Our `Boolean` does not have this `Show` ability yet.

### Our New Type Fails: The "Eq" Error

Let's try to compare our new values.

```hs
λ> No == No
<interactive>:75:4: error: [GHC-39999]
    • No instance for ‘Eq Boolean’ arising from a use of ‘==’
    ...
```

It fails again!\
Haskell says: "I have `No instance for 'Eq'`.\
`Eq` (Equality) is the "characteristic" of being comparable.\
Our `Boolean` doesn't have this ability either.

Let's ask Haskell *what* the `Eq` rule is:

```hs
λ> :info Eq
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```

This tells us everything we need to know:

- `class Eq a where`: This defines the "Equality" rule
- It has two functions: `==` (equals) and `/=` (not equals)
- `{-# MINIMAL (==) | (/=) #-}`: This is the important part.\
  It means, "To follow this rule, you **must** implement *at least one* of these functions (either `==` or `/=`)"

If you write `==`, the compiler will automatically write `/=` for you.\
If you write `/=`, the compiler will write `==` for you.

Our `Boolean` type doesn't follow this rule yet, so Haskell doesn't know how to compare `No == No`.

### Type Classes: A "Characteristic" is a Rule

This brings us to the most important idea in Haskell: **Type Classes**.

A Type Class is **a rule that a type must follow.**\
It asks: "Does this type have a certain ability? Yes or No?"

Let's use our teacher's `sort` example.\
We have a function $sort :: [A] \to [A]$.\
Is this definition correct?

> **NO!**

Why?\
Because the set `A` is too *general*.\
We can't sort everything in the real world:

```hs
[Flower, Door, Jug, Soil, Sand, Big Tree]
```

In Haskell, we cannot sort a list of functions:

```hs
[isAlpha, isDigit, isSpace]
```

This means our math rule is **wrong**:

$$sort: \forall l \in [A] \to [A]$$

This is NOT true "for all" `A`.

We need a *constraint*.\
`A` must have the characteristic of **Order**.

The correct type for `sort` is:

$$sort: \text{Ord }A \implies [A] \to [A]$$


> [!note]
> This reads: "If type A has the Ord characteristic, then sort can turn a `[A]` into a `[A]`."

The same is true for `unique` (in Haskell, `nub`).\
`nub` doesn't need "Order" (it doesn't care if `A < B`).\
It only needs "Equality" (does A == B?).

$$unique: \text{Eq }A \implies [A] \to [A]$$

That is why we see these types in GHCi:

```hs
λ> :type sort
sort :: Ord a => [a] -> [a]

λ> :type nub
nub :: Eq a => [a] -> [a]
```

#### Why we can't compare functions

Why can't our list of functions `[isAlpha, isDigit, isSpace]` have Eq?\
To know if two functions $f$ and $f'$ are equal, we must prove:

$$f = f' \implies \forall x, f(x) = f'(x)$$

This means we must test **every possible input `x`** (which could be infinite!).\
We saw how long it took to test 1 billion numbers for `factors`.\
It is impossible to test an infinite set.\
That is why, in a computer, functions cannot have an `Eq` characteristic.

> [!tip]
> (When we talk about this, we must shift our mindset from the "value-level" to the "type-level".)

#### `Eq` is the Foundation for `Ord`

`Eq` and `Ord` are related.

- `Eq` (Equality) just has one job: `==`
- `Ord` (Order) has jobs like `<` or `<=`

But think about this: $A \le B$ is just $(A < B) \lor (A = B)$.\
The $=$ is a sub-problem of $\le$.\
This means "Equality" is a foundation for "Order".\
A type that can be Ordered must *first* be able to be Equaled.\
The Eq set is a "superset" of the Ord set.

### The Solution: Fixing Our `Boolean` Type

Now we know *why* our `Boolean` failed.\
It needs `Show` and `Eq`.\
Let's give them to it.

#### Method 1: The Hard Way (Manual Instance)

We can "prove" to Haskell that `Boolean` follows the rules.

**1. Fixing `Eq`:** The `Eq` type class requires a function `==` or `/=`.\
We will write `==`.

```
λ> :{
λ| instance Eq Boolean where
λ|   (==) :: Boolean -> Boolean -> Bool
λ|   Yes == Yes = True
λ|   No  == No  = True
λ|   _   == _   = False
λ| :}
```

Now, it works!

```
λ> No == No
True

λ> No /= Yes
True
```

**2. Fixing `Show`:** The `Show` type class requires a function `show`.

```
λ> :{
λ| instance Show Boolean where
λ|   show :: Boolean -> String
λ|   show Yes = "Yes"
λ|   show No  = "No"
λ| :}
```

Now, it prints!

```hs
λ> Yes
Yes

λ> No
No
```

#### Method 2: The Easy Way (`deriving`)

That was a lot of work.\
For simple types like `Eq` and `Show`, we can just ask the compiler to write that code for us using `deriving`.

```hs
λ> data Boolean' = Yes' | No' deriving (Eq, Show)
```

That's it!\
Just one line.\
Now, it works perfectly.

```hs
λ> Yes'
Yes'

λ> No'
No'

λ> Yes' == Yes'
True

λ> Yes' /= No'
True
```

### The `Ord` (Order) Type Class

We fixed `Eq` (Equality) and `Show` (Printing).\
But what if we want to `sort` our `Boolean` type?\
We would need to add the `Ord` characteristic.

How can we define Order (`Ord`)?\
Let's see the `Ord` information:

```hs
λ> :info Ord
type Ord :: * -> Constraint
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
```

This looks like a lot of functions.\
But, just like `Eq`, we can look at the `MINIMAL` tag.\
It says we only need to write **one** function: either `compare` or `<=`.\
If we write one of them, the compiler will automatically write all the others (`<`, `>`, `max`, `min`, etc.) for us.

Now, does it make sense to `sort` our `Boolean` type?\
Is `Yes` greater than `No`?
Maybe, but it's not a great example.\
Let's use a new type where order is very clear: days of the week.

We will call it `Day`, and we'll `derive` `Eq` and `Show` like we learned.

```hs
λ> :{
λ| data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
λ|   deriving (Eq, Show)
λ| :}
```

We can print it and check its type:

```hs
λ> Mon
Mon
λ> :type Mon
Mon :: Day
```

Now, let's try to `sort` a list of days.

```hs
λ> days = [Mon, Wed, Tue, Fri, Thu]
λ> sort days
<interactive>:583:1: error: [GHC-39999]
    • No instance for 'Ord Day'
    ...
```

As we expected, it fails!\
The `Day` type doesn't have the `Ord` (Order) characteristic yet.

We *could* write the `instance` by hand.\
But we would have to tell Haskell that `Mon <= Tue`, `Mon <= Wed`, `Mon <= Thu`, `Tue <= Wed`, `Tue <= Thu`...\
That is a *lot* of work, even for only 7 values!\
I bet you don't wanna imagine to a ton of possible values, or even infinite values, right!?

Fortunately, Haskell knows how to `derive` `Ord` for us.\
It will use the order that we wrote in the `data` definition (`Mon`, then `Tue`, then `Wed`...).

```hs
λ> :{
λ| data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
λ|   deriving (Eq, Ord, Show)
λ| :}
```

We just added `Ord` to the `deriving` list.\
Now, let's try to sort again.

```hs
λ> days = [Mon, Wed, Tue, Fri, Thu]

λ> sort days
[Mon,Tue,Wed,Thu,Fri]
```

It works perfectly!

> [!warning]
> The compiler can write `deriving` code for us, but it's not magic.\
> This only works for simple types.\
> It doesn't mean the compiler can do this for *every* situation.

We will learn more about those other cases soon.

---

## Creating Our Own Type Classes

We know about built-in Type Classes like `Show`, `Eq`, and `Ord`.\
But what if we want to define our *own* characteristic?

Let's start with some shapes.

### 1. Defining `Circle` and `Square`

We can define a `Circle` that holds a radius (`Float`).\
And a `Square` that holds a side length (`Float`).

```hs
λ> newtype Circle = Circle Float deriving (Eq, Show)
λ> newtype Square = Square Float deriving (Eq, Show)
```

We can create them easily:

```hs
λ> c = Circle 10.0
λ> s = Square 10.0
```

### 2. The Problem: Name Collision

We want to calculate the **area** for both shapes.\
Let's try to write an `area` function for each.

```hs
-- This will FAIL!
area :: Circle -> Float
area (Circle r) = pi * r * r

area :: Square -> Float
area (Square s) = s * s
```

**Error!** Haskell says:\
"Multiple declarations of 'area'".\
We cannot have two functions with the same name.\
Haskell does not support "Function Overloading" like Java or C++.

### 3. The Solution: Make a Type Class

Instead of overloading, we define a **Generic Type**.\
We create a new "characteristic" called `HasArea`.

```hs
λ> :{
λ| class HasArea a where
λ|   area :: a -> Float
λ| :}
```

This reads: "A type `a` has the `HasArea` characteristic *if* it has a function `area` that turns `a` into a `Float`."

Now we can implement this rule for our shapes:

```hs
λ> :{
λ| instance HasArea Circle where
λ|   area (Circle r) = pi * r * r
λ|
λ| instance HasArea Square where
λ|   area (Square s) = s * s
λ| :}
```

Now, we can use `area` on both types!

```hs
λ> area (Circle 10)
314.15927

λ> area (Square 10)
100.0
```

This is the power of Type Classes.\
We can use the same function name (`area`) for different types, as long as they share the same characteristic (`HasArea`).

---

## Making Data Clearer: Record Syntax

Let's look at another problem with complex data types.

### The Mystery Data Problem

Imagine we have an API Endpoint type with 2 strings.

```hs
data ApiEndpoint = ApiEndpoint String String deriving (Show)
```

When we use it:

```hs
ApiEndpoint "https://base-api.com" "/api/v1/users"
-- or --
ApiEndpoint "GET" "api/v1/users"
```

Which string is which?\
Is the first one the Method or the URL?\
It is hard to know.

It gets worse with more fields.\
Imagine a `Score` type:

```hs
data Score = Score Int Int [Int] [Int] deriving (Show)
```

If we see: `Score 78 81 [10, 20] [5, 5]`.

- What is 78? (Midterm?)
- What is 81? (Final?)
- What are the lists? (Homeworks? Projects?)

We can't guess.\
This is confusing.

### Solution 1: Accessor Functions

The way to be able to let us know is creating accessor functions to extract them:

```hs
λ> :{
λ| midterm :: Score -> Int
λ| midterm (Score m _ _ _) = m
λ|
λ| final :: Score -> Int
λ| final (Score _ f _ _) = f
λ|
λ| projects :: Score -> [Int]
λ| projects (Score _ _ ps _) = ps
λ|
λ| homeworks :: Score -> [Int]
λ| homeworks (Score _ _ _ hs) = hs
λ| :}
```

But this way is very exhausted.\
Let’s see another solution.

### Solution 2: Record Syntax

Haskell has a feature called **Record Syntax**.\
It lets us name each field.

Haskell

```
λ> :{
λ| data Score = Score
λ|   { midterm   :: Int
λ|   , final     :: Int
λ|   , projects  :: [Int]
λ|   , homeworks :: [Int]
λ|   } deriving (Show)
λ| :}
```

Now, when we print it, we see the names!

```hs
λ> Score 78 81 [10, 20] [5, 5]
Score {midterm = 78, final = 81, projects = [10,20], homeworks = [5,5]}
```

And the best part? Haskell automatically creates **accessor functions** for us.

```hs
λ> score = Score 78 81 [10, 20] [5, 5]

λ> midterm score
78

λ> homeworks score
[5,5]
```

This makes our code much easier to read and use.

---

## Recursive Type: Functor and The Generalized Container

### 1. Building Natural Numbers (`Nat`)

We can build natural numbers using two parts:

- **`Zero`**: The number 0.
- **`Succ Nat`**: The successor (next number) of another `Nat`.

```hs
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)
```

| **`Nat` Value**    | **Means** |
| ------------------ |:---------:|
| `Zero`             | 0         |
| `Succ Zero`        | 1         |
| `Succ (Succ Zero)` | 2         |

We must write our own math functions like `add` because Haskell cannot automatically give the `Num` (Number) ability to our custom `Nat` type.

```hs
λ> :{
λ| add :: Nat -> Nat -> Nat
λ| m `add` Zero = m
λ| m `add` (Succ n) = Succ (m `add` n)
λ| :}
```

This way, we prove how addition works for our new type.

### 2. Building Our Own List (`List a`)

The built-in list (`[a]`) is also a recursive type.\
We can make our own version:

```hs
data List a = EmptyList | Cons a (List a)
```

- **`EmptyList`**: The list is empty (`[]`)
- **`Cons a (List a)`**: Put a value `a` at the front, attached to the rest of the list (`List a`)

`Cons` is just like the `(:)` operator used to connect elements in Haskell's built-in lists.

---

## The Power of Functors

We often want to **change the values** inside a structure (like a list) without changing the structure itself.\
For a list, we use `map`.

But `map` only works for the built-in list type (`[a]`).

### What is a Functor?

A **Functor** is a **Type Class** (a set of rules) that describes any structure that can be **"mapped over"**.\
It is a way to generalize the `map` function.

- The key function is **`fmap`**.

```hs
λ> :type fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

This means: If you have a function that changes `a` to `b` (`a -> b`), `fmap` can use it to change an entire **Functor of `a`** (`f a`) into a **Functor of `b`** (`f b`).

It works for:

- Built-in Lists (`[]`)
- Optional values (`Maybe`)
- Our own `List a`
- And many other types!

### Implementing Functor for Our `List`

To let `fmap` work on our custom `List a`, we must tell Haskell how to do it:

```hs
λ> :{
λ| instance Functor List where
λ|   fmap _ EmptyList   = EmptyList
λ|   fmap f (Cons x xs) = Cons (f x) (fmap f xs)
λ| :}
```

1. If the list is `EmptyList`, the result is `EmptyList`
2. If it's `Cons x xs`, apply the function `f` to the first item (`x`), then call `fmap` on the rest of the list (`xs`)

Now, we can use `fmap` to double the numbers in our custom list:

```hs
λ> fmap (*2) (Cons 100 (Cons 20 EmptyList))
Cons 200 (Cons 40 EmptyList)
```

### Functors are General

The best part is that once you define a function using `fmap`, it works on **any Functor**:

```hs
λ> mapDouble = fmap (*2)

λ> mapDouble [1..5]     -- Built-in List
[2,4,6,8,10]

λ> mapDouble (Just 10)  -- Maybe (Optional)
Just 20

λ> mapDouble $ Cons 10 (Cons 20 EmptyList) -- Our custom List
Cons 20 (Cons 40 EmptyList)
```

For simple types like `Optional a`, the compiler can even write the `fmap` code for you using `deriving Functor`.

---

## Binary Tree: Another Recursive Type

We can apply the same recursive logic to a **Binary Tree**.

<div align=center>
  <img width="2580" height="1600" alt="image" src="https://github.com/user-attachments/assets/a62665f1-246f-468f-8b09-7e77ad5831f6" />
</div>

A Binary Tree is recursive because a "Branch" of a tree is just another small tree.

```hs
λ> :{
λ| data BinaryTree a = EmptyTree
λ|   | Node
λ|     { value :: a
λ|     , left  :: BinaryTree a
λ|     , right :: BinaryTree a
λ|     }
λ|   deriving (Eq, Show)
λ| :}
```

- **`EmptyTree`**: The tree has no data.
- **`Node`**: It holds a value `a` and two sub-trees (`left` and `right`).

### Helper Function: Single Node

It is hard to type `EmptyTree` every time.

Let's make a helper function to create a "Leaf" (a node with no children).

```hs
λ> :{
λ| singleNode :: a -> BinaryTree a
λ| singleNode x = Node x EmptyTree EmptyTree
λ| :}
```

### 1. Making it a Functor

We want to change values inside the tree (like `map` for a List).\
So, we define the **Functor** instance.

The logic is simple:

1. Apply function `f` to the current `value`.
2. Recursively call `fmap` on the `left` tree.
3. Recursively call `fmap` on the `right` tree.

```hs
λ> :{
λ| instance Functor BinaryTree where
λ|   fmap _ EmptyTree = EmptyTree
λ|   fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
λ| :}
```

Now we can use `fmap` to change every value in the tree at once!

## Building the Tree: `insertTree`

We need a way to put data into the tree.\
We will use the logic of a **Binary Search Tree (BST)**.

The Logic:

Let $x$ be the new value, and $v$ be the current node value.

$$x < v \implies \text{Go Left}$$

$$x > v \implies \text{Go Right}$$

$$x = v \implies \text{Do nothing (No duplicates)}$$

Here is the code:

```hs
λ> :{
λ| insertTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
λ| insertTree x EmptyTree = singleNode x
λ| insertTree x (Node v left right)
λ|   | x == v = Node v left right             -- Equal? Keep same.
λ|   | x < v  = Node v (insertTree x left) right  -- Less? Insert Left.
λ|   | x > v  = Node v left (insertTree x right)  -- More? Insert Right.
λ| :}
```

**Let's test it:**

```hs
λ> t1 = singleNode 17
-- Root is 17

λ> t2 = insertTree 12 t1
-- 12 < 17, so 12 goes to Left
-- Node {value = 17, left = Node {value = 12 ...}, ...}

λ> t3 = insertTree 21 t2
-- 21 > 17, so 21 goes to Right
-- Node {value = 17, left = ..., right = Node {value = 21 ...}}
```

## Smart Construction: Using `foldr`

Inserting items one by one is slow and messy.\
We want to turn a `List` of numbers into a `Tree` automatically.

We can use **`foldr`**.

> [!tip]
> Think of `foldr` as a **Generalized Loop**.

### Understanding the Type

Let's look at the type of `foldr`:

```hs
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

This looks complex.\
Let's map it to our Tree problem.\
We want to take a List (`t a`) and turn it into a Tree (`b`).

1. **Input (`a`)**: A number from our list
2. **Accumulator (`b`)**: Our `BinaryTree`
3. **Function (`a -> b -> b`)**: A function that takes a number and a tree, and returns a new tree

**Wait!**

That is _exactly_ our `insertTree` function!

```hs
insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
-- Matches ::         (a ->      b       ->      b)
```

So, `foldr` will loop through our list and call `insertTree` for every number.

### The `mkTree` Function

We can define a smart constructor called `mkTree`.

```hs
λ> :{
λ| mkTree :: (Ord a) => [a] -> BinaryTree a
λ| mkTree = foldr insertTree EmptyTree
λ| :}
```

How it works:

`foldr` processes the list from Right to Left.\
Example: `mkTree [1, 3, 2]`

1. Start with `EmptyTree`.
2. Take `2`: `insertTree 2 EmptyTree` $\to$ Tree with 2.
3. Take `3`: `insertTree 3 (Tree 2)` $\to$ 3 > 2, so 3 goes Right.
4. Take `1`: `insertTree 1 (Tree 2)` $\to$ 1 < 2, so 1 goes Left.

**Result:**

```hs
λ> mkTree [18, 9, 9, 7, 23, 19]
Node {value = 19, left = Node {value = 7 ...} ...}
```

This is much cleaner than calling `insertTree` manually!

## The Problem: We can't count it

We have a tree.\
But we cannot ask simple questions about it.

```
λ> t1 = mkTree [18, 9, 7, 23, 19]

λ> length t1
<interactive>:1106:1: error: [GHC-39999]
    • No instance for ‘Foldable BinaryTree’ ...
```

Why?\
Because `length` requires the **`Foldable`** characteristic.

```hs
λ> :type length
length :: Foldable t => t a -> Int
```

Haskell knows how to fold a List (`[]`).\
But it does not know how to fold our `BinaryTree`.

### The Solution: `instance Foldable`

To be `Foldable`, we must explain how to "walk" through our tree.

We will implement `foldr`.

**The Logic:**

1. Fold the **Right** side first.
2. Process the **Current** value.
3. Fold the **Left** side last.

(This order creates a sorted list!)

```hs
λ> :{
λ| instance Foldable BinaryTree where
λ|   foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
λ|   foldr _ acc EmptyTree = acc
λ|   foldr f acc (Node x left right) = 
λ|      let acc_right = foldr f acc right    -- 1. Fold Right
λ|          acc_node  = f x acc_right        -- 2. Apply function to Value
λ|      in  foldr f acc_node left            -- 3. Fold Left
λ| :}
```

> [!tip]
> You can write it in one line: `foldr f e (Node x l r) = foldr f (f x (foldr f e r)) l`

### The Magic of Type Classes

Now that we added the `Foldable` characteristic, we get **superpowers** for free.

We didn't write `sum`.\
We didn't write `maximum`.\
We didn't write `length`.

But they all work now!

```hs
-- Count nodes
λ> length t1
5

-- Find the biggest number
λ> maximum t1
23

-- Sum all numbers
λ> sum t1
76

-- Turn tree back into a list (Sorted!)
λ> toList t1
[7, 9, 18, 19, 23]
```

This is why Haskell is powerful.\
We define the **Structure** (Data).\
We define the **Characteristic** (Type Class).\
And Haskell gives you the **Functions** for free.
