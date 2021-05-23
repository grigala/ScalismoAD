package ch.grigala.scalismoad.rule

trait MathRuleBreeze[U[_], V[_], T] extends ValueRuleBreeze[U, V, T] {


    // element-wise sin(DenseMatrix[Double])
    def sin1(v: U[T]): U[T]

    def sin2(v: V[T]): V[T]

    def sin3(v: T): T

    def cos1(v: U[T]): U[T]

    def cos2(v: V[T]): V[T]

    def cos3(v: T): T

    def tan1(v: U[T]): U[T]

    def tan2(v: V[T]): V[T]

    def tan3(v: T): T


    def asin1(v: U[T]): U[T]

    def asin2(v: V[T]): V[T]

    def asin3(v: T): T


    def acos1(v: U[T]): U[T]

    def acos2(v: V[T]): V[T]

    def acos3(v: T): T


    def atan1(v: U[T]): U[T]

    def atan2(v: V[T]): V[T]

    def atan3(v: T): T


    def sinh1(v: U[T]): U[T]

    def sinh2(v: V[T]): V[T]

    def sinh3(v: T): T

    def cosh1(v: U[T]): U[T]

    def cosh2(v: V[T]): V[T]

    def cosh3(v: T): T

    def tanh1(v: U[T]): U[T]

    def tanh2(v: V[T]): V[T]

    def tanh3(v: T): T
}
