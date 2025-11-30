# Day 5 - Functor, Applicative, and Monad

<img src="../../../images/map-apply-bind.png" alt="Diagram to explain Map-Apply-Bind for Functor, Applicative, and Monad">

**Concept Mapping from the Board:**

| Type Class | Function Shape | Input Context | Operation |
| :--- | :--- | :--- | :--- |
| **Functor** | `(a -> b)` | `f a` | `fmap` / `<$>` |
| **Applicative** | `f (a -> b)` | `f a` | `apply` / `<*>` |
| **Monad** | `(a -> f b)` | `f a` | `bind` / `>>=` |

---

## Inscribed Shapes Fractal Problem

<img src="../../../images/inscribed-shapes-fractal.png" alt="Inscribed Shapes Fractal (Framed Rose)">

---

## fmap (`<$>`) and apply (`<*>`)

### 1. Functor: fmap (`<$>`)

We have a **normal function** and a **value in a context**.

```hs
λ> :type (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b

λ> :type fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

**Usage:**

```hs
λ> fmap (*2) [1, 10, 100, 1000]
[2,20,200,2000]

λ> (*2) <$> [1, 10, 100, 1000]
[2,20,200,2000]
```

---

### 2. Applicative: apply (`<*>`)

Now, even the **function is in a context**.

```hs
λ> :type (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

**The List Context:** Why does it produce a longer list?\
In Functional Programming, a `List` isn't just a container; it represents **non-determinism** or **possibilities**.

When we use `<*>`, we are combining **every possible function** with **every possible value**.

```hs
λ> [(*2), (*3), (*4)] <*> [10, 100, 1000]
[20,200,2000,30,300,3000,40,400,4000]
```

**Let's trace the logic:**

We apply every function on the left to every value on the right (Cartesian Product).

1. Apply `(*2)` to `[10, 100, 1000]`:
    - 10 * 2 = 20
    - 100 * 2 = 200
    - 1000 * 2 = 2000

2. Apply `(*3)` to `[10, 100, 1000]`:
    - 10 * 3 = 30
    - 100 * 3 = 300
    - 1000 * 3 = 3000

3. Apply `(*4)` to `[10, 100, 1000]`:
    - 10 * 4 = 40
    - 100 * 4 = 400
    - 1000 * 4 = 4000

**Conclusion:**

Yes, we get the result applied for **all possible combinations** of values.
