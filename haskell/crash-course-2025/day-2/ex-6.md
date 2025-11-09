# Exercise 6

Write two functions for Caesar Cipher: `caesarCipher :: Int -> String -> String` and `caesarDecipher :: Int -> String -> String`.\

The functions change letters by moving them forward or backward in the alphabet.\
Keep numbers the same.\
Do not move symbols or spaces.


```hs
λ> caesarCipher 4 ['a'..'z']
"efghijklmnopqrstuvwxyzabcd"

λ> caesarDecipher 4 it
"abcdefghijklmnopqrstuvwxyz"

λ> caesarCipher 6 "Dave Rawitat"
"Jgbk Xgcozgz"

λ> caesarDecipher 6 "Jgbk Xgcozgz"
"Dave Rawitat"

λ> caesarDecipher 8 "Jgbk Xgcozgz"
"Bytc Pyugryr"

λ> caesarCipher 2 "3215697"
"3215697"

λ> caesarCipher 2 "!!!!!@^&#%@@&*I#"
"!!!!!@^&#%@@&*K#"
```
