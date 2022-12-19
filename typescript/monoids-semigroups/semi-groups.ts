type SemiGroup<T = any> = (x: T) => {
    x: T
    concat: (o: ReturnType<SemiGroup<T>>) => ReturnType<SemiGroup<T>>
    toString: () => string
}

const Sum: SemiGroup = (x) => ({
    x,
    concat: (other) => Sum(x + other.x),
    toString: () => `Sum(${x})`
})

const Product: SemiGroup<number> = (x) => ({
    x,
    concat: (other) => Product(x * other.x),
    toString: () => `Product(${x})`
})

const Any: SemiGroup = (x) => ({
    x,
    concat: (other) => Any(x || other.x),
    toString: () => `Any(${x})`
})

const All: SemiGroup = (x) => ({
    x,
    concat: (other) => All(x && other.x),
    toString: () => `All(${x})`
})

console.log(Sum(3).concat(Sum(2)).toString()) // Sum(5)
console.log(Sum(5).toString(), '\n===============') // Same as recent line

console.log(Product(3).concat(Product(4)).toString()) // Sum(12)
console.log(Product(12).toString(), '\n===============') // Same as recent line

console.log(Any(false).concat(Any(true)).toString()) // Product(true)
console.log(Any(true).toString(), '\n===============') // Same as recent line
console.log(Any(true).concat(Any(false)).toString()) // Product(true)
console.log(Any(true).toString(), '\n===============') // Same as recent line
console.log(Any(false).concat(Any(false)).toString()) // Product(false)
console.log(Any(false).toString(), '\n===============') // Same as recent line

console.log(All(true).concat(All(true)).toString()) // All(true)
console.log(All(true).toString(), '\n===============') // Same as recent line
console.log(All(true).concat(All(false)).toString()) // All(false)
console.log(All(false).toString(), '\n===============') // Same as recent line
console.log(All(false).concat(All(true)).toString()) // All(false)
console.log(All(false).toString(), '\n===============') // Same as recent line
console.log(All(false).concat(All(false)).toString()) // All(false)
console.log(All(false).toString(), '\n===============') // Same as recent line
