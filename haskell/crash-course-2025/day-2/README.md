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
