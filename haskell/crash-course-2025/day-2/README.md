# Day 2

## FizzBuzz with infinite lists

We will make FizzBuzz in Haskell using functional programming.\
For numbers from 1 to infinity:
- Print "Fizz" when the number can be divided by 3
- Print "Buzz" when the number can be divided by 5
- Print "FizzBuzz" when the number can be divided by both 3 and 5

> [!info]
> This is not the fastest way, but it shows another way to think in Functional Programming.

### Building the Solution

We build the answer step by step from smaller parts.

**Step 1: Make the pattern for Fizz**

```
Fizz     = _  _  F  _  _  F  _  _  F  _  _  F  _  _  F  ...
```
Every 3rd position shows "F" (for Fizz), other positions are empty.

**Step 2: Make the pattern for Buzz**

```
Buzz     = _  _  _  _  B  _  _  _  _  B  _  _  _  _  B  ...
```
Every 5th position shows "B" (for Buzz), other positions are empty.

**Step 3: Put Fizz and Buzz together**
```
FzBz     = _  _  F  _  B  F  _  _  F  B  _  F  _  _ FB  ...
```
When we combine them, position 15 shows "FB" (FizzBuzz) because it's both 3rd and 5th.

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

### The `Zip` Function

Think about zipping two lists together like a zipper on clothes.\
Or buttoning two lists together like shirts.

```hs
λ> zip [1..10] ['a'..'j']
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i'),(10,'j')]
```

### The `ZipWith` Function

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
-- x is member of the set 1-10 where x is even, and we double it
λ> [ x * 2 | x <- [1..10], even x ]
[4,8,12,16,20]

-- x is member of the set 1-100 where x squared is less than 50
λ> [ x | x <- [1..100], x * x < 50 ]
[1,1,2,3,4,5,6,7]

-- x is member of the set 1-10 where x squared is less than 50, and we double it
λ> [x * 2 | x <- [1..10], x * x < 50]
[2,4,6,8,10,12,14]

-- w is member of the words in the given string where length of w is at least 3
λ> [w | w <- words "this is a cat this is a bat that is a rat", length w >= 3]
["this","cat","this","bat","that","rat"]
```
