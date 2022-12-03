function twice<T>(func: (arg: T) => T) {
    return function (arg: T) {
        return func(func(arg))
    }
}
// twice((x: number) => x + x)(5) //?

function thrice<T>(func: (arg: T) => T) {
    return function (arg: T) {
        return func(func(func(arg)))
    }
}
// thrice((x: number) => x + x)(5) //?
type PredicateFunction = (...args: any[]) => boolean
function filter<TPredicate extends PredicateFunction>(predicate: TPredicate) {
    return function forArr(xs: Parameters<TPredicate>[0][]) {
        return xs.filter(predicate)
    }
}

function filter$<T, TPredicate extends PredicateFunction>(predicate: TPredicate) {
    return function forArr([x, ...xs]: Parameters<TPredicate>[number][]): T[] {
        if (xs.length === 0) return predicate(x as Parameters<TPredicate>[0])
        return filter(predicate)(xs)
    }
}

const filterByEven = filter((x: number) => x % 2 === 0)
const filterByEven$ = filter$((x) => x % 2 === 0)
console.log(filterByEven([1,2,3,4]))
console.log(filterByEven$([1,2,3,4]))
