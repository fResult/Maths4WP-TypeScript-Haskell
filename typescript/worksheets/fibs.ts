import {chain, compose,} from 'radash'
import { filter, map, reduce } from '../list/list-operator'

function fibs(n: number) {
    return Array.from({ length: n })
        .reduce<number[]>((acc, _, i) => {
            return [...acc, i < 2 ? i : acc[i - 2] + acc[i-1]]
        }, [])
}
// fibs(40) //?

function add(y: number) {
    return function forX(x: number) {
        return x + y
    }
}


const addOne = add(1)
const addTwo = add(2)

const chained: (xs: number[]) => number = chain(
    map(addTwo),
    map(addOne),
    map(addTwo),
    filter(x => x % 2  === 0),
    reduce<number, number>((acc, x) => acc + x, 0),
    addTwo
)

chained([1,2,3,4])//?
