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
        static empty(): Monoid {
            throw Error("Empty doesn't Implement Error")
        }
    }

    class Min extends Monoid<number> {
        private constructor(protected x: number) {
            super(x)
        }

        public static just(x: number): Min {
            return new Min(x)
        }

        public concat(other: this): Monoid<number> {
            return (this.x < other.x
                ? new Min(this.x)
                : new Min(other.x)) as unknown as Monoid<number>
        }

        public toString(): string {
            return `${Min.name}(${this.x})`
        }

        public static empty() {
            return Min.just(Infinity)
        }
    }

    class Max extends Monoid<number> {
        private constructor(protected x: number) {
            super(x)
        }

        public static just(x: number): Max {
            return new Max(x)
        }

        public concat(other: this): Monoid<number> {
            return (this.x > other.x
                ? Max.just(this.x)
                : Max.just(other.x)) as unknown as Monoid<number>
        }

        public toString(): string {
            return `${Min.name}(${this.x})`
        }

        public static empty(): Max {
            return Max.just(-Infinity)
        }
    }

    console.log(Min.just(12).concat(Min.just(5)).concat(Min.empty()).concat(Min.empty()))
    console.log(Max.just(12).concat(Max.just(5)).concat(Max.just(15)).concat(Max.empty()))
})()
