function isPrimeImperative(n: number): boolean {
    if (n <= 1) {
        return false;
    }
    for (let i = 2; i < n / 2; i++) {
        if (n % i === 0) {
            return false;
        }
    }
    return true
}
isPrimeImperative(7) //?

function isPrimeFp1(n: number) {
    const list1toN: number[] = Array.from({ length: n }, (_, i) => i + 1)
        .filter(x => n % x == 0)
    return list1toN.length === 2
}
isPrimeFp1(7) //?

function isPrimeFp2(n: number) {
    if (n <= 1) return false
    if ([2,3].includes(n)) return true
    const list1toN: number[] = Array.from({ length: n }, (_, i) => i+1)
    let countLoop = 0
    return !list1toN.slice(1, list1toN.length - 1).some(x => {
        console.log(x % x === 0 && ++countLoop)
        return n % x === 0
    })
}
