import { compose } from 'radash';
import { map } from '../list/list-operator';
// twice :: (a -> a) -> a -> a
function twice<T>(func: (arg: T) => T) {
    return function (arg: T) {
        return func(func(arg))
    }
}

twice((x: number) => x + x)(5) //?

function thrice<T>(func: (arg: T) => T) {
    return function (arg: T) {
        return func(func(func(arg)))
    }
}

thrice((x: number) => x + x)(5) //?