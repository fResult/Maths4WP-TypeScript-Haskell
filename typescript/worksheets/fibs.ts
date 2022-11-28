import {compose, map, reduce} from 'radash'

function fibs(n: number) {
    return Array.from({ length: n })
        .reduce<number[]>((acc, _, i) => {
            return [...acc, i < 2 ? i : acc[i - 2] + acc[i-1]]
        }, [])
}
// fibs(40) //?


function addOne(x: number) {
    return x + 1
}
function addTwo(x: number) {
    return x + 2
}

const composed = compose(
    addOne,
    addTwo
)

composed(1) //?