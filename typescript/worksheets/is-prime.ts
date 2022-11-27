function isPrimeImperative(n: number): boolean {
    if (n <= 1) {
        return false;
    }
    for (let i = 2; i < n / 2; i++) {
        if (n % i === 0) {
            return false;
        }
    }
    return true;
}
isPrimeImperative(5) //?

function isPrimeFP(n: number) {
    return Array.from({ length: n }, (_, i) => i + 1).filter(x => n % x == 0).length === 2
}
isPrimeFP(7) //?
