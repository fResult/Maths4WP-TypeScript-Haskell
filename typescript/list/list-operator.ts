export function zip(a: number[], b: number[]) {
    return a.map((x, i) => [x, b[i]])
}

export function tail<T>(arr: T[]) {
    return arr.slice(1)
}
