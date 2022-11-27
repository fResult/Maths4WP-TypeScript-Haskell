function fibs(n: number) {
    return Array.from({ length: n })
        .reduce<number[]>((acc, _, i) => {
            return [...acc, i < 2 ? i : acc[i - 2] + acc[i-1]]
        }, [])
}
fibs(99) //?
