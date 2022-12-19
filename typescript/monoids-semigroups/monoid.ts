;(function MonoidImplementation() {
    type SemiGroup<T = any> = (x: T) => {
        x: T
        concat: (o: ReturnType<SemiGroup<T>>) => ReturnType<SemiGroup<T>>
        toString: () => string
    }

    type Monoid<T = any> = SemiGroup<T> & {
        empty: () => ReturnType<Monoid<T>>
    }

    const Sum: Monoid = (x) => ({
        x,
        concat: (other) => Sum(x + other.x),
        toString: () => `Sum(${x})`
    })
    Sum.empty = () => Sum(0)

    const Product: Monoid<number> = (x) => ({
        x,
        concat: (other) => Product(x * other.x),
        toString: () => `Product(${x})`
    })
    Product.empty = () => Product(1)

    const Any: Monoid = (x) => ({
        x,
        concat: (other) => Any(x || other.x),
        toString: () => `Any(${x})`
    })
    Any.empty = () => Any(false)

    const All: Monoid = (x) => ({
        x,
        concat: (other) => All(x && other.x),
        toString: () => `All(${x})`
    })
    All.empty = () => All(true)

    // USAGES
    const resultAll = [true, true, true]
        .map(All)
        .reduce((acc, n) => acc.concat(n), All.empty())
    console.log(resultAll.toString()) //?

    const resultAny = [true, false, true]
        .map(Any)
        .reduce((acc, bool) => acc.concat(bool), Any.empty())
    console.log(resultAny.toString()) //?

    const resultSum = [1, 3, 5, 7, 9]
        .map(Sum)
        .reduce((acc, n) => acc.concat(n), Sum.empty())
    console.log(resultSum.toString()) //?

    // Implement Monoids more
    const Min: Monoid<number> = (x) => ({
        x,
        concat: (other) => x < other.x ? Min(x) : Min(other.x),
        toString: () => `Min(${x})`
    })
    Min.empty = () => Min(Infinity)

    type SemiGroupPair<T = any, S = any> = (x: T, y: S) => {
        fst: T,
        snd: S,
        concat: (o: ReturnType<SemiGroupPair<T, S>>) => ReturnType<SemiGroupPair<T, S>>,
        toString: () => string
    }
    type MonoidPair<T = any, S = any> = SemiGroupPair<T, S> & {
        empty: () => ReturnType<MonoidPair<T, S>>
    }
    const Tuple: MonoidPair = (fst, snd) => ({
        fst,
        snd,
        concat: (other) => Tuple(fst.concat(other.fst), snd.concat(other.snd)),
        toString: () => `Tuple(${fst}, ${snd})`
    })
    Tuple.empty = () => Tuple(null, null)

    // Usages more
    const resultMin = [5, 3, 4, 2, 7]
        .map(Min)
        .reduce((acc, n) => acc.concat(n), Min.empty())
    console.log(resultMin.toString()) //?

    console.log(
        Tuple(
            Sum(1),
            Product(2)
        ).concat(
            Tuple(
                Sum(5),
                Product(2)
            )
        ).toString()
    ) //?

})()
