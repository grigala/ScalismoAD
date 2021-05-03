package ch.grigala.scalismoad.rule

trait MathRule[U[_], T] extends ValueRule[U, T] {

    def sinS(v: U[T]): U[T]

    def cosS(v: U[T]): U[T]

    def tanS(v: U[T]): U[T]

    def asinS(v: U[T]): U[T]

    def acosS(v: U[T]): U[T]

    def atanS(v: U[T]): U[T]

    def sinhS(v: U[T]): U[T]

    def coshS(v: U[T]): U[T]

    def tanhS(v: U[T]): U[T]

    def logS(v: U[T]): U[T]

    def expS(v: U[T]): U[T]

    def absS(v: U[T]): U[T]

    def sqrtS(v: U[T]): U[T]

    def sinM(v: T): T

    def cosM(v: T): T

    def tanM(v: T): T

    def asinM(v: T): T

    def acosM(v: T): T

    def atanM(v: T): T

    def sinhM(v: T): T

    def coshM(v: T): T

    def tanhM(v: T): T

    def lnM(v: T): T

    def expM(v: T): T

    def absM(v: T): T

    def sqrtM(v: T): T

    def powSS(v: U[T], p: U[T]): U[T]

    def powSM(v: U[T], p: T): U[T]

    def powMS(v: T, p: U[T]): U[T]

    def powMM(v: T, p: T): T

    def dotSS(v: U[T], p: U[T]): U[T]

    def dotSM(v: U[T], p: T): U[T]

    def dotMS(v: T, p: U[T]): U[T]

    def dotMM(v: T, p: T): T
}


