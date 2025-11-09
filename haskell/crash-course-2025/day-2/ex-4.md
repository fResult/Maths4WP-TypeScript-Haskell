# Exercise 4

Data Compression is a very important task in computer science.\
We will write a compression function.


```hs
compress :: String -> String
```

This function uses simple string compression by replacing consecutive repeated characters with the character followed by the count, for example:

```hs
λ> compress "AAAAAAABBBBAAAACCCC"
"A7B4A4C4"

λ> compress "AAAAAAAAAAAAAAAAAAAAAAAAA"
"A25"

λ> compress "ABC"
"A1B1C1"
```
