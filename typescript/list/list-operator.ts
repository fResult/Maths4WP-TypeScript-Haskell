type PredicateFunction = (...args: any[]) => boolean

export function zip(a: number[], b: number[]) {
    return a.map((x, i) => [x, b[i]])
}

export function tail<T>(arr: T[]) {
    return arr.slice(1)
}

export function reduce<T, U>(reducerFn: (acc: U, item: T, idx?: number) => U, initialValue: U) {
    return function forArray(arr: T[]) {
        return arr.reduce(reducerFn, initialValue!)
    }
}

export function filter<TPredicate extends PredicateFunction>(predicateFn: TPredicate) {
    return function fromArray<T extends Parameters<TPredicate>[0]>(arr: T[]) {
        return arr.filter(predicateFn)
    }
}

export function map<T, S>(mapperFn: (item: T, idx?: number) => S) {
    return function forArray(arr: any[]) {
        return arr.map(mapperFn)
    }
}
