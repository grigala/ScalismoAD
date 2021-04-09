package ch.grigala.ad.rule

import breeze.linalg.{DenseMatrix, DenseVector, InjectNumericOps}
import ch.grigala.ad.graph._
import ch.grigala.ad.rule._

import scala.language.implicitConversions


object BreezeRule {

    type T = Double
    type V = DenseVector[T]
    type M = DenseMatrix[T]

    trait DenseVectorValueRule extends ValueRule[DenseVector, T] {

        override def zeroM: T = 0.0

        override def zeroS(shape: V): V = DenseVector.zeros[T](shape.data.size)

        override def oneM: T = 1.0

        override def oneS(shape: V): V = DenseVector.ones[T](shape.data.size)

        override def addSS(l: V, r: V): V = l + r

        override def mulSS(l: V, r: V): V = l *:* r

        override def divSS(l: V, r: V): V = l /:/ r

        override def addSM(l: V, r: T): V = l + r

        override def mulSM(l: V, r: T): V = l * r

        override def divSM(l: V, r: T): V = l / r

        override def addMS(l: T, r: V): V = r + l

        override def mulMS(l: T, r: V): V = r *:* l

        override def divMS(l: T, r: V): V = r.map(l / _)

        override def addMM(l: T, r: T): T = l + r

        override def mulMM(l: T, r: T): T = l * r

        override def divMM(l: T, r: T): T = l / r

        override def ltSS(l: V, r: V): DenseVector[Boolean] = (l <:< r).map(b => b)

        override def lteSS(l: V, r: V): DenseVector[Boolean] = (l <:= r).map(b => b)

        override def gtSS(l: V, r: V): DenseVector[Boolean] = (l >:> r).map(b => b)

        override def gteSS(l: V, r: V): DenseVector[Boolean] = (l >:= r).map(b => b)

        override def eqSS(l: V, r: V): DenseVector[Boolean] = (l :== r).map(b => b)

        override def ltSM(l: V, r: T): DenseVector[Boolean] = l.map(_ < r)

        override def lteSM(l: V, r: T): DenseVector[Boolean] = l.map(_ <= r)

        override def gtSM(l: V, r: T): DenseVector[Boolean] = l.map(_ > r)

        override def gteSM(l: V, r: T): DenseVector[Boolean] = l.map(_ >= r)

        override def eqSM(l: V, r: T): DenseVector[Boolean] = l.map(_ == r)

        override def ltMS(l: T, r: V): DenseVector[Boolean] = r.map(l < _)

        override def lteMS(l: T, r: V): DenseVector[Boolean] = r.map(l <= _)

        override def gtMS(l: T, r: V): DenseVector[Boolean] = r.map(l > _)

        override def gteMS(l: T, r: V): DenseVector[Boolean] = r.map(l >= _)

        override def eqMS(l: T, r: V): DenseVector[Boolean] = r.map(l == _)

        override def ltMM(l: T, r: T): Boolean = l < r

        override def lteMM(l: T, r: T): Boolean = l <= r

        override def gtMM(l: T, r: T): Boolean = l > r

        override def gteMM(l: T, r: T): Boolean = l >= r

        override def eqMM(l: T, r: T): Boolean = l == r

        override def posS(v: V): V = v

        override def negS(v: V): V = -v

        override def posM(v: T): T = v

        override def negM(v: T): T = -v

        override def transposeS(v: V): V = v

        override def transposeM(v: T): T = v

        override def closeSS(l: V, r: V, eps: T = 1e-4): DenseVector[Boolean] = {
            (breeze.numerics.abs(subSS(l, r)) >:= eps).map(b => b)
        }

        override def subSS(l: V, r: V): V = l - r

        override def closeSM(l: V, r: T, eps: T = 1e-4): DenseVector[Boolean] = {
            (breeze.numerics.abs(subSM(l, r)) >:= eps).map(b => b)
        }

        override def subSM(l: V, r: T): V = l - r

        override def closeMS(l: T, r: V, eps: T = 1e-4): DenseVector[Boolean] = {
            (breeze.numerics.abs(subMS(l, r)) >:= eps).map(b => b)
        }

        override def subMS(l: T, r: V): V = r.map(l - _)

        override def closeMM(l: T, r: T, eps: T = 1e-4): Boolean = {
            breeze.numerics.abs(subMM(l, r)) <= eps
        }

        override def subMM(l: T, r: T): T = l - r

        override def whereSSS(cond: DenseVector[Boolean], a: V, b: V): V = breeze.linalg.where(cond, a, b)

        override def whereSSM(cond: DenseVector[Boolean], a: V, b: T): V = breeze.linalg.where(cond, a, fillLike(b, cond))

        override def whereSMS(cond: DenseVector[Boolean], a: T, b: V): V = breeze.linalg.where(cond, fillLike(a, cond), b)

        override def whereSMM(cond: DenseVector[Boolean], a: T, b: T): V = breeze.linalg.where(cond, fillLike(a, cond), fillLike(b, cond))

        override def whereMSS(cond: Boolean, a: V, b: V): V = if (cond) a else b

        override def whereMSM(cond: Boolean, a: V, b: T): V = if (cond) a else fillLike(b, a)

        override def whereMMS(cond: Boolean, a: T, b: V): V = if (cond) fillLike(a, b) else b

        private[this] def fillLike[A](value: T, ref: DenseVector[A]): V = DenseVector.fill(ref.length, value)

        override def whereMMM(cond: Boolean, a: T, b: T): T = if (cond) a else b
    }

    trait DenseVectorMathRule extends MathRule[DenseVector, T] {

        override def sinS(v: V): V = breeze.numerics.sin(v)

        override def cosS(v: V): V = breeze.numerics.cos(v)

        override def tanS(v: V): V = breeze.numerics.tan(v)

        override def asinS(v: V): V = breeze.numerics.asin(v)

        override def acosS(v: V): V = breeze.numerics.acos(v)

        override def atanS(v: V): V = breeze.numerics.atan(v)

        override def sinhS(v: V): V = breeze.numerics.sinh(v)

        override def coshS(v: V): V = breeze.numerics.cosh(v)

        override def tanhS(v: V): V = breeze.numerics.tanh(v)

        override def lnS(v: V): V = breeze.numerics.log(v)

        override def expS(v: V): V = breeze.numerics.exp(v)

        override def absS(v: V): V = breeze.numerics.abs(v)

        override def sqrtS(v: V): V = breeze.numerics.sqrt(v)

        override def sinM(v: T): T = breeze.numerics.sin(v)

        override def cosM(v: T): T = breeze.numerics.cos(v)

        override def tanM(v: T): T = breeze.numerics.tan(v)

        override def asinM(v: T): T = breeze.numerics.asin(v)

        override def acosM(v: T): T = breeze.numerics.acos(v)

        override def atanM(v: T): T = breeze.numerics.atan(v)

        override def sinhM(v: T): T = breeze.numerics.sinh(v)

        override def coshM(v: T): T = breeze.numerics.cosh(v)

        override def tanhM(v: T): T = breeze.numerics.tanh(v)

        override def lnM(v: T): T = breeze.numerics.log(v)

        override def expM(v: T): T = breeze.numerics.exp(v)

        override def absM(v: T): T = breeze.numerics.abs(v)

        override def sqrtM(v: T): T = breeze.numerics.sqrt(v)

        // TODO: fix this implicit arguent error
        override def powSS(v: V, p: V): V = breeze.numerics.pow(v, p)

        override def powSM(v: V, p: T): V = breeze.numerics.pow(v, p)

        override def powMS(v: T, p: V): V = breeze.numerics.pow(v, p)

        override def powMM(v: T, p: T): T = breeze.numerics.pow(v, p)

        override def dotSS(a: V, b: V): V = DenseVector(a dot b) // FIXME

        override def dotSM(a: V, b: T): V = a *:* b

        override def dotMS(a: T, b: V): V = a *:* b

        override def dotMM(a: T, b: T): T = a *:* b
    }

    trait DenseMatrixValueRule extends ValueRule[DenseMatrix, T] {

        override def zeroM: T = 0.0

        override def zeroS(shape: M): M = DenseMatrix.zeros[T](shape.rows, shape.cols)

        override def oneM: T = 1.0

        override def oneS(shape: M): M = DenseMatrix.ones[T](shape.rows, shape.cols)

        override def addSS(l: M, r: M): M = l + r

        override def mulSS(l: M, r: M): M = l *:* r

        override def divSS(l: M, r: M): M = l /:/ r

        override def addSM(l: M, r: T): M = l + r

        override def mulSM(l: M, r: T): M = l * r

        override def divSM(l: M, r: T): M = l / r

        override def addMS(l: T, r: M): M = r + l

        override def mulMS(l: T, r: M): M = r *:* l

        override def divMS(l: T, r: M): M = r.map(l / _)

        override def addMM(l: T, r: T): T = l + r

        override def mulMM(l: T, r: T): T = l * r

        override def divMM(l: T, r: T): T = l / r

        override def ltSS(l: M, r: M): DenseMatrix[Boolean] = l <:< r

        override def lteSS(l: M, r: M): DenseMatrix[Boolean] = l <:= r

        override def gtSS(l: M, r: M): DenseMatrix[Boolean] = l >:> r

        override def gteSS(l: M, r: M): DenseMatrix[Boolean] = l >:= r

        override def eqSS(l: M, r: M): DenseMatrix[Boolean] = l :== r

        override def ltSM(l: M, r: T): DenseMatrix[Boolean] = l.map(_ < r)

        override def lteSM(l: M, r: T): DenseMatrix[Boolean] = l.map(_ <= r)

        override def gtSM(l: M, r: T): DenseMatrix[Boolean] = l.map(_ > r)

        override def gteSM(l: M, r: T): DenseMatrix[Boolean] = l.map(_ >= r)

        override def eqSM(l: M, r: T): DenseMatrix[Boolean] = l.map(_ == r)

        override def ltMS(l: T, r: M): DenseMatrix[Boolean] = r.map(l < _)

        override def lteMS(l: T, r: M): DenseMatrix[Boolean] = r.map(l <= _)

        override def gtMS(l: T, r: M): DenseMatrix[Boolean] = r.map(l > _)

        override def gteMS(l: T, r: M): DenseMatrix[Boolean] = r.map(l >= _)

        override def eqMS(l: T, r: M): DenseMatrix[Boolean] = r.map(l == _)

        override def ltMM(l: T, r: T): Boolean = l < r

        override def lteMM(l: T, r: T): Boolean = l <= r

        override def gtMM(l: T, r: T): Boolean = l > r

        override def gteMM(l: T, r: T): Boolean = l >= r

        override def eqMM(l: T, r: T): Boolean = l == r

        override def posS(v: M): M = v

        override def negS(v: M): M = -v

        override def posM(v: T): T = v

        override def negM(v: T): T = -v

        override def transposeS(v: M): M = v.t

        override def transposeM(v: T): T = v

        override def closeSS(l: M, r: M, eps: T = 1e-4): DenseMatrix[Boolean] = {
            (breeze.numerics.abs(subSS(l, r)) <:= eps).map(b => b)
        }

        override def subSS(l: M, r: M): M = l - r

        override def closeSM(l: M, r: T, eps: T = 1e-4): DenseMatrix[Boolean] = {
            (breeze.numerics.abs(subSM(l, r)) <:= eps).map(b => b)
        }

        override def subSM(l: M, r: T): M = l - r

        override def closeMS(l: T, r: M, eps: T = 1e-4): DenseMatrix[Boolean] = {
            (breeze.numerics.abs(subMS(l, r)) <:= eps).map(b => b)
        }

        override def subMS(l: T, r: M): M = r.map(l - _)

        override def closeMM(l: T, r: T, eps: T = 1e-4): Boolean = {
            breeze.numerics.abs(subMM(l, r)) <= eps
        }

        override def subMM(l: T, r: T): T = l - r

        override def whereSSS(cond: DenseMatrix[Boolean], a: M, b: M): M = breeze.linalg.where(cond, a, b)

        override def whereSSM(cond: DenseMatrix[Boolean], a: M, b: T): M = breeze.linalg.where(cond, a, fillLike(b, cond))

        override def whereSMS(cond: DenseMatrix[Boolean], a: T, b: M): M = breeze.linalg.where(cond, fillLike(a, cond), b)

        private[this] def fillLike[A](value: T, ref: DenseMatrix[A]): M = DenseMatrix.fill(ref.rows, ref.cols)(value)

        override def whereSMM(cond: DenseMatrix[Boolean], a: T, b: T): M = breeze.linalg.where(cond, fillLike(a, cond), fillLike(b, cond))

        override def whereMSS(cond: Boolean, a: M, b: M): M = if (cond) a else b

        override def whereMSM(cond: Boolean, a: M, b: T): M = if (cond) a else fillLike(b, a)

        override def whereMMS(cond: Boolean, a: T, b: M): M = if (cond) fillLike(a, b) else b

        override def whereMMM(cond: Boolean, a: T, b: T): T = if (cond) a else b
    }

    trait DenseMatrixMathRule extends MathRule[DenseMatrix, T] {

        override def sinS(v: M): M = breeze.numerics.sin(v)

        override def cosS(v: M): M = breeze.numerics.cos(v)

        override def tanS(v: M): M = breeze.numerics.tan(v)

        override def asinS(v: M): M = breeze.numerics.asin(v)

        override def acosS(v: M): M = breeze.numerics.acos(v)

        override def atanS(v: M): M = breeze.numerics.atan(v)

        override def sinhS(v: M): M = breeze.numerics.sinh(v)

        override def coshS(v: M): M = breeze.numerics.cosh(v)

        override def tanhS(v: M): M = breeze.numerics.tanh(v)

        override def lnS(v: M): M = breeze.numerics.log(v)

        override def expS(v: M): M = breeze.numerics.exp(v)

        override def absS(v: M): M = breeze.numerics.abs(v)

        override def sqrtS(v: M): M = breeze.numerics.sqrt(v)

        override def sinM(v: T): T = breeze.numerics.sin(v)

        override def cosM(v: T): T = breeze.numerics.cos(v)

        override def tanM(v: T): T = breeze.numerics.tan(v)

        override def asinM(v: T): T = breeze.numerics.asin(v)

        override def acosM(v: T): T = breeze.numerics.acos(v)

        override def atanM(v: T): T = breeze.numerics.atan(v)

        override def sinhM(v: T): T = breeze.numerics.sinh(v)

        override def coshM(v: T): T = breeze.numerics.cosh(v)

        override def tanhM(v: T): T = breeze.numerics.tanh(v)

        override def lnM(v: T): T = breeze.numerics.log(v)

        override def expM(v: T): T = breeze.numerics.exp(v)

        override def absM(v: T): T = breeze.numerics.abs(v)

        override def sqrtM(v: T): T = breeze.numerics.sqrt(v)

        // TODO
        override def powSS(v: M, p: M): M = breeze.numerics.pow(v, p)

        override def powSM(v: M, p: T): M = breeze.numerics.pow(v, p)

        override def powMS(v: T, p: M): M = breeze.numerics.pow(v, p)

        override def powMM(v: T, p: T): T = breeze.numerics.pow(v, p)

        override def dotSS(a: M, b: M): M = a * b

        override def dotSM(a: M, b: T): M = a *:* b

        override def dotMS(a: T, b: M): M = a *:* b

        override def dotMM(a: T, b: T): T = a *:* b
    }

    class DenseVectorRule extends DenseVectorValueRule with DenseVectorMathRule

    class DenseMatrixRule extends DenseMatrixValueRule with DenseMatrixMathRule

    object Implicits {

        implicit val denseVectorRule = new DenseVectorRule
        implicit val denseMatrixRule = new DenseMatrixRule

        // Literal conversion for constructing computational tree
        implicit def fromByte(v: Byte): ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)

        implicit def fromShort(v: Short): ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)

        implicit def fromInt(v: Int): ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)

        implicit def fromLong(v: Long): ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)

        implicit def fromFloat(v: Float): ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)

        implicit def fromDouble(v: Double): ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v)
    }

}
