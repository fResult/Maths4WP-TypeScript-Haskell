// See the definition in the Category Theory 2 slide (Math for Programmers Day 6) - Page 636
// - SemiGroup is a Set with an Associative Binary Operation (concat)
// - Monoid is a SemiGroup with an Identity Element

// Monoid Object which is a triple (M, c, u) in a Monoidal Category
// - Monoidal Category (C, ⊗, I) is a Category with a Bifunctor ⊗ and an Identity Object I
// - M is an Object in the Category
// - c is a Morphism from M ⊗ M to M (c: M ⊗ M → M)
// - u is a Morphism from I to M (u: I → M)
// - I is the Identity Object or the Unit Object in the Category
// - ⊗ is the Tensor Product or the Monoidal Product
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

    // Nat Number is Monoid under Addition is (N, +, 0)
    class Addition extends Monoid<number> {
        private constructor(protected x: number) {
            super(x)
        }

        public static just(x: number): Addition {
            return new Addition(x)
        }

        public concat(other: Addition): Addition {
            return Addition.just(this.x + other.x)
        }

        public static empty() {
            return Addition.just(0)
        }

        public toString(): string {
            return `${Addition.name}(${this.x})`
        }
    }

    // Nat Number is Monoid under Multiplication is (N, ×, 1)
    class Multiplication extends Monoid<number> {
        private constructor(protected x: number) {
            super(x)
        }

        public static just(x: number): Multiplication {
            return new Multiplication(x)
        }

        public concat(other: Multiplication): Multiplication {
            return Multiplication.just(this.x * other.x)
        }

        public static empty() {
            return Multiplication.just(1)
        }

        public toString(): string {
            return `${Multiplication.name}(${this.x})`
        }
    }

    // Nat Number is Monoid under Min is (N, min, ∞)
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

    // Nat Number is Monoid under Max is (N, max, -∞)
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

    // List is Monoid under Concatenation is (List[T], concat, [])
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

    // Boolean is Monoid under And is (Bool, &&, True)
    class And extends Monoid<boolean> {
        private constructor(protected x: boolean) {
            super(x)
        }

        public static just(x: boolean): And {
            return new And(x)
        }

        public concat(other: And): And {
            return And.just(this.x && other.x)
        }

        public static empty(): And {
            return And.just(true)
        }

        public toString(): string {
            return `${And.name}(${this.x})`
        }
    }

    // Boolean is Monoid under Or is (Bool, ||, False)
    class Or extends Monoid<boolean> {
        private constructor(protected x: boolean) {
            super(x)
        }

        public static just(x: boolean): Or {
            return new Or(x)
        }

        public concat(other: Or): Or {
            return Or.just(this.x || other.x)
        }

        public static empty(): Or {
            return Or.just(false)
        }

        public toString(): string {
            return `${Or.name}(${this.x})`
        }
    }

    console.log(Addition.just(1).concat(Addition.just(2)).concat(Addition.just(3)).concat(Addition.empty()).toString())
    console.log(Multiplication.just(12).concat(Multiplication.just(5)).concat(Multiplication.empty()).toString())

    console.log(Min.just(12).concat(Min.just(5)).concat(Min.empty()).concat(Min.empty()).toString())
    console.log(Max.just(12).concat(Max.just(5)).concat(Max.just(15)).concat(Max.empty()).toString())

    console.log(List.just(["A", "B"]).concat(List.just(["B", "C"])).concat(List.empty()).concat(List.just(["C", "D"])).toString())

    console.log(And.just(false).concat(And.just(false)).concat(And.just(true)).concat(And.empty()).concat(And.just(false)).toString())
    console.log(Or.just(false).concat(Or.just(false)).concat(Or.just(true)).concat(Or.empty()).concat(Or.just(false)).toString())
})()
