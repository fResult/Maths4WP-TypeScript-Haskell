;(function MonoidClassesImplementation() {
    abstract class MySemiGroup<T> {
        constructor(protected x: T) {}

        public abstract concat(other: MySemiGroup<T>): MySemiGroup<T>
        public abstract toString(): string
    }

    abstract class Monoid<T = any> extends MySemiGroup<T> {
        constructor(protected x: T) {
            super(x)
        }
        static empty(): Monoid {
            throw Error("Empty doesn't Implement Error")
        }
    }

    class MyMin extends Monoid<number> {
        private constructor(protected x: number) {
            super(x)
        }

        public static just(x: number): MyMin {
            return new MyMin(x)
        }

        public concat(other: this): Monoid<number> {
            return (this.x < other.x
                ? new MyMin(this.x)
                : new MyMin(other.x)) as unknown as Monoid<number>
        }

        public toString(): string {
            return `${MyMin.name}(${this.x})`
        }

        public static empty() {
            return MyMin.just(Infinity)
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
            return `${MyMin.name}(${this.x})`
        }

        public static empty(): Max {
            return Max.just(-Infinity)
        }
    }

    console.log(MyMin.just(12).concat(MyMin.just(5)).concat(MyMin.empty()).concat(MyMin.empty()))
    console.log(Max.just(12).concat(Max.just(5)).concat(Max.just(15)).concat(Max.empty()))
})()
