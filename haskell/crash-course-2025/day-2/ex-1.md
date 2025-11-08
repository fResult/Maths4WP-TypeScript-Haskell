# Exercise 1

Try out these functions from `Data.List` Library:

1. [`length`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:length)
2. [`group`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:group)
3. [`nub`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:nub)
4. [`filter`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:filter)
5. [`head`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:head)
6. [`tail`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:tail)
7. [`init`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:init)
8. [`last`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:last)
9. [`reverse`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:reverse)
10. [`concat`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:concat)
11. [`map`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:map)
12. [`concatMap`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:concatMap)
13. [`any`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:any)
14. [`all`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:all)
15. [`and`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:and)
16. [`or`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:or)
17. [`sum`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:sum)
18. [`product`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:product)
19. [`maximum`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:maximum)
20. [`minimum`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:minimum)
21. [`iterate`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:iterate)
22. [`repeat`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:repeat)
23. [`replicate`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:replicate)
24. [`cycle`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:cycle)
25. [`take`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:take)
26. [`drop`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:drop)
27. [`takeWhile`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:takeWhile)
28. [`dropWhile`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:dropWhile)
29. [`span`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:span)
30. [`elem`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:elem)
31. [`zip`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:zip)
32. [`unzip`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:unzip)
33. [`delete`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:delete)
34. [`intersect`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intersect)
35. [`intersperse`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intersperse)
36. [`intercalate`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intercalate)
37. [`permutations`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:permutations)

And write these functions by yourself as many as you can (you can name them with `'` after the name like `length'` or with `my` before the name like `mylength`).

> [!note]
> **Note 1:**
>
> All functions you write **do not need to care about efficiency** - they don't have to be the best.\
> Really, you don't need to care at all.\
> And write them yourself only.\
> For ones you can't do, can't think of, just skip them.\
> Don't search for code on the net or ask AI.

> [!note]
> **Note 2:**
>
> In case a function has Type constraint `Foldable t => t a` instead of `[a]` like `length :: Foldable t a => Int`, you can use `[a]` instead in the version you write yourself.

> [!note]
> **Note 3:**
>
> Functions you write yourself don't need to have the same result as the Library version in cases where many answers are equal.\
> For example, `permutations` don't need to come out in the same order of sequences, as long as you get all permutations the same (like permutations function for `"ab"` can answer `["ba", "ab"]`.
> Or in case of `group` that you make yourself, you might not need to keep the order of things that appear, as long as groups are right).

> [!note]
> **Note 4:**
>
> Some functions need to use other functions from this list to help (that is, some problems when you break them down, you will find some that are in this list).\
> So before you think about writing, play with them first.

> [!note]
> **Note 5:**
>
> Finally... if you can't write any at all, it's okay.\
> At least play with them (but I think this won't happen... because some functions like `head`, `tail` are very easy).

*Ref:* https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html
