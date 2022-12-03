/**
  * @author Thai Pangsakulyanont's post on Facebook
  */
// ! still wrong
pleaseFind(
    allTheNumbersInRange(0, 9999),
    which(sumOf(eachDigit, toThePowerOf(4)), equals(itself))
) //?

function pleaseFind(r: number, f: (x: number) => number[]): void {
    for (let val of f(r)) {
        console.log(val)
    }
}

function* allTheNumbersInRange(from: number, to: number) {
    for (let i = from; i <= to; i++) {
        yield i
    }
} //?

function which(m: Function, c: Function) {
    return function* (r: number) {
        for (let val of r) {
            if (c(val)(m(val))) {
                yield val
            }
        }
    }
}

function sumOf(l: (x: number) => number[], m: (x: number) => number) {
    return function (val: number) {
        return l(val).map(m).reduce((acc, x) => acc + x)
    }
}

function eachDigit(val: number) {
    return [...val.toString()].map(x => +x)
}

function toThePowerOf(k: number) {
    return function (val: number) {
        return val ** k
    }
}

function equals(t: (x: number) => number) {
    return function (val: number) {
        return function (x: number) {
            return t(val) === x
        }
    }
}

function itself(x: number) {
    return x
}
