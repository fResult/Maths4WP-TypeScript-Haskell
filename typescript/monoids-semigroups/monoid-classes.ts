// See the definition in the Category Theory 2 slide (Math for Programmers Day 6) - Page 636
// - SemiGroup is a Set with an Associative Binary Operation (concat)
// - Monoid is a SemiGroup with an Identity Element
;(function MonoidClassesImplementation() {
    abstract class SemiGroup<T> {
        constructor(protected x: T) {}

        public abstract concat(other: SemiGroup<T>): SemiGroup<T>
        public abstract toString(): string
    }

    abstract class Monoid<T = any> extends SemiGroup<T> {
        constructor(protected x: T) {
            super(x)
        }
        // Identity Element
        static empty(): Monoid {
            throw Error("Empty doesn't Implement Error")
        }
    }

    class Add extends Monoid<number> {
        private constructor(protected x: number) {
            super(x)
        }

        public static just(x: number): Add {
            return new Add(x)
        }

        public concat(other: Add): Add {
            return Add.just(this.x + other.x)
        }

        public static empty() {
            return Add.just(0)
        }

        public toString(): string {
            return `${Add.name}(${this.x})`
        }
    }

    class Multiply extends Monoid<number> {
        private constructor(protected x: number) {
            super(x)
        }

        public static just(x: number): Multiply {
            return new Multiply(x)
        }

        public concat(other: Multiply): Multiply {
            return Multiply.just(this.x * other.x)
        }

        public static empty() {
            return Multiply.just(1)
        }

        public toString(): string {
            return `${Multiply.name}(${this.x})`
        }
    }

    class Min extends Monoid<number> {
        private constructor(protected x: number) {
            super(x)
        }

        public static just(x: number): Min {
            return new Min(x)
        }

        public concat(other: Min): Min {
            return this.x < other.x ?  Min.just(this.x): Min.just(other.x)
        }

        public static empty() {
            return Min.just(Infinity)
        }

        public toString(): string {
            return `${Min.name}(${this.x})`
        }
    }

    class Max extends Monoid<number> {
        private constructor(protected x: number) {
            super(x)
        }

        public static just(x: number): Max {
            return new Max(x)
        }

        public concat(other: Max): Max {
            return this.x > other.x ? Max.just(this.x) : Max.just(other.x)
        }

        public static empty(): Max {
            return Max.just(-Infinity)
        }

        public toString(): string {
            return `${Max.name}(${this.x})`
        }
    }

    class List<T = any> extends Monoid<T[]> {
        private constructor(protected x: T[]) {
            super(x)
        }

        public static just<T>(xs: T[]): List<T> {
            return new List(xs)
        }

        public concat(other: List<T>): List<T> {
            return List.just(this.x.concat(other.x))
        }

        public static empty<T>(): List<T> {
            return List.just([])
        }

        public toString(): string {
            return `${List.name}(${this.x.join(", ")})`
        }
    }

    console.log(Add.just(1).concat(Add.just(2)).concat(Add.just(3)).concat(Add.empty()).toString())
    console.log(Multiply.just(12).concat(Multiply.just(5)).concat(Multiply.empty()).toString())
    console.log(Min.just(12).concat(Min.just(5)).concat(Min.empty()).concat(Min.empty()).toString())
    console.log(Max.just(12).concat(Max.just(5)).concat(Max.just(15)).concat(Max.empty()).toString())
    console.log(List.just(["A", "B"]).concat(List.just(["B", "C"])).concat(List.empty()).concat(List.just(["C", "D"])).toString())
})()
