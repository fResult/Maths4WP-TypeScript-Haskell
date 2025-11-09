# Exercise 3

Stalin Sort is a nonsensical sorting algorithm based on Stalinist purges - the basic idea is to "eliminate everyone who is out of place".\
Therefore, Stalin Sort of any list will eliminate elements that are out of order.

Write the function:

```hs
stalinSort :: (Ord a) => [a] -> [a]
```

With the following working example:

```hs
λ> stalinSort [3,1,2,4,3,7,1,9,10,11,2,4]
[3,4,7,9,10,11]
```

