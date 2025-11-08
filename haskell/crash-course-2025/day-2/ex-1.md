# Exercise 1

Try out these functions from `Data.List` Library:

- [`length`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:length)
- [`group`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:group)
- [`nub`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:nub)
- [`filter`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:filter)
- [`head`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:head)
- [`tail`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:tail)
- [`init`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:init)
- [`last`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:last)
- [`reverse`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:reverse)
- [`concat`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:concat)
- [`map`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:map)
- [`concatMap`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:concatMap)
- [`any`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:any)
- [`all`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:all)
- [`and`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:and)
- [`or`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:or)
- [`sum`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:sum)
- [`product`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:product)
- [`maximum`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:maximum)
- [`minimum`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:minimum)
- [`iterate`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:iterate)
- [`repeat`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:repeat)
- [`replicate`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:replicate)
- [`cycle`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:cycle)
- [`take`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:take)
- [`drop`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:drop)
- [`takeWhile`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:takeWhile)
- [`dropWhile`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:dropWhile)
- [`span`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:span)
- [`elem`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:elem)
- [`zip`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:zip)
- [`unzip`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:unzip)
- [`delete`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:delete)
- [`intersect`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intersect)
- [`intersperse`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intersperse)
- [`intercalate`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intercalate)
- [`permutations`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:permutations)

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
