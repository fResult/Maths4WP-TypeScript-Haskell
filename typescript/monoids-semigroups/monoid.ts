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

    // console.log(Sum(3).concat(Sum(2)).toString()) // Sum(5)
    // console.log(Sum(5).toString(), '\n===============') // Same as recent line

    // console.log(Product(3).concat(Product(4)).toString()) // Sum(12)
    // console.log(Product(12).toString(), '\n===============') // Same as recent line

    // console.log(Any(false).concat(Any(true)).toString()) // Product(true)
    // console.log(Any(true).toString(), '\n===============') // Same as recent line
    // console.log(Any(true).concat(Any(false)).toString()) // Product(true)
    // console.log(Any(true).toString(), '\n===============') // Same as recent line
    // console.log(Any(false).concat(Any(false)).toString()) // Product(false)
    // console.log(Any(false).toString(), '\n===============') // Same as recent line

    // console.log(All(true).concat(All(true)).toString()) // All(true)
    // console.log(All(true).toString(), '\n===============') // Same as recent line
    // console.log(All(true).concat(All(false)).toString()) // All(false)
    // console.log(All(false).toString(), '\n===============') // Same as recent line
    // console.log(All(false).concat(All(true)).toString()) // All(false)
    // console.log(All(false).toString(), '\n===============') // Same as recent line
    // console.log(All(false).concat(All(false)).toString()) // All(false)
    // console.log(All(false).toString(), '\n===============') // Same as recent line

    const resAll = [true, true, true]
        .map(All)
        .reduce((acc, n) => acc.concat(n), All.empty())
    console.log(resAll.toString()) //?
})()
