package ch.grigala.scalismoad.rule

import breeze.linalg.{DenseMatrix, DenseVector, InjectNumericOps}
import ch.grigala.scalismoad.graph._

import scala.language.implicitConversions


object BreezeRule {

    type D = Double
    type DV = DenseVector[D]
    type DM = DenseMatrix[D]

    trait DenseVectorValueRule extends ValueRule[DenseVector, D] {
        //        def adjustSize(left: DV, right: DV): (DV, DV) = {
        //            var ll = left
        //            var rr = right
        //
        //            if (left.size < right.size) {
        //                ll = DenseVector.zeros[Double](right.size)
        //                ll(0, left.size) := left
        //            } else if (left.size > right.size) {
        //                rr = DenseVector.zeros[Double](left.size)
        //                rr(0, right.size) := right
        //            }
        //            (ll, rr)
        //        }

        override def zeroM: D = 0.0

        override def zeroC(shape: DV): DV = DenseVector.zeros[D](shape.data.length)

        override def oneM: D = 1.0

        override def oneC(shape: DV): DV = DenseVector.ones[D](shape.data.length)

        override def addCC(l: DV, r: DV): DV = l + r

        override def mulCC(l: DV, r: DV): DV = {
            //            val (ll, rr) = adjustSize(l, r)
            //            println(ll)
            //            println(rr)
            l *:* r
        }

        override def divCC(l: DV, r: DV): DV = l /:/ r

        override def addCM(l: DV, r: D): DV = l + r

        override def mulCM(l: DV, r: D): DV = l * r

        override def divCM(l: DV, r: D): DV = l / r

        override def addMC(l: D, r: DV): DV = r + l

        override def mulMC(l: D, r: DV): DV = r *:* l

        override def divMC(l: D, r: DV): DV = r.map(l / _)

        override def addMM(l: D, r: D): D = l + r

        override def mulMM(l: D, r: D): D = l * r

        override def divMM(l: D, r: D): D = l / r

        override def ltCC(l: DV, r: DV): DenseVector[Boolean] = (l <:< r).map(b => b)

        override def lteCC(l: DV, r: DV): DenseVector[Boolean] = (l <:= r).map(b => b)

        override def gtCC(l: DV, r: DV): DenseVector[Boolean] = (l >:> r).map(b => b)

        override def gteCC(l: DV, r: DV): DenseVector[Boolean] = (l >:= r).map(b => b)

        override def eqCC(l: DV, r: DV): DenseVector[Boolean] = (l :== r).map(b => b)

        override def ltCM(l: DV, r: D): DenseVector[Boolean] = l.map(_ < r)

        override def lteCM(l: DV, r: D): DenseVector[Boolean] = l.map(_ <= r)

        override def gtCM(l: DV, r: D): DenseVector[Boolean] = l.map(_ > r)

        override def gteCM(l: DV, r: D): DenseVector[Boolean] = l.map(_ >= r)

        override def eqCM(l: DV, r: D): DenseVector[Boolean] = l.map(_ == r)

        override def ltMC(l: D, r: DV): DenseVector[Boolean] = r.map(l < _)

        override def lteMC(l: D, r: DV): DenseVector[Boolean] = r.map(l <= _)

        override def gtMC(l: D, r: DV): DenseVector[Boolean] = r.map(l > _)

        override def gteMC(l: D, r: DV): DenseVector[Boolean] = r.map(l >= _)

        override def eqMC(l: D, r: DV): DenseVector[Boolean] = r.map(l == _)

        override def ltMM(l: D, r: D): Boolean = l < r

        override def lteMM(l: D, r: D): Boolean = l <= r

        override def gtMM(l: D, r: D): Boolean = l > r

        override def gteMM(l: D, r: D): Boolean = l >= r

        override def eqMM(l: D, r: D): Boolean = l == r

        override def posC(v: DV): DV = v

        override def negC(v: DV): DV = -v

        override def posM(v: D): D = v

        override def negM(v: D): D = -v

        override def transposeC(v: DV): DV = v

        override def transposeM(v: D): D = v

        override def closeCC(l: DV, r: DV, eps: D = 1e-4): DenseVector[Boolean] = {
            (breeze.numerics.abs(subCC(l, r)) >:= eps).map(b => b)
        }

        override def subCC(l: DV, r: DV): DV = l - r

        override def closeCM(l: DV, r: D, eps: D = 1e-4): DenseVector[Boolean] = {
            (breeze.numerics.abs(subCM(l, r)) >:= eps).map(b => b)
        }

        override def subCM(l: DV, r: D): DV = l - r

        override def closeMC(l: D, r: DV, eps: D = 1e-4): DenseVector[Boolean] = {
            (breeze.numerics.abs(subMC(l, r)) >:= eps).map(b => b)
        }

        override def subMC(l: D, r: DV): DV = r.map(l - _)

        override def closeMM(l: D, r: D, eps: D = 1e-4): Boolean = {
            breeze.numerics.abs(subMM(l, r)) <= eps
        }

        override def subMM(l: D, r: D): D = l - r

        override def whereCCC(cond: DenseVector[Boolean], a: DV, b: DV): DV = breeze.linalg.where(cond, a, b)

        override def whereCCM(cond: DenseVector[Boolean], a: DV, b: D): DV = breeze.linalg.where(cond, a, fillLike(b, cond))

        override def whereCMC(cond: DenseVector[Boolean], a: D, b: DV): DV = breeze.linalg.where(cond, fillLike(a, cond), b)

        override def whereCMM(cond: DenseVector[Boolean], a: D, b: D): DV = breeze.linalg.where(cond, fillLike(a, cond), fillLike(b, cond))

        override def whereMCC(cond: Boolean, a: DV, b: DV): DV = if (cond) a else b

        override def whereMCM(cond: Boolean, a: DV, b: D): DV = if (cond) a else fillLike(b, a)

        override def whereMMC(cond: Boolean, a: D, b: DV): DV = if (cond) fillLike(a, b) else b

        private[this] def fillLike[A](value: D, ref: DenseVector[A]): DV = DenseVector.fill(ref.length, value)

        override def whereMMM(cond: Boolean, a: D, b: D): D = if (cond) a else b
    }

    trait DenseVectorMathRule extends MathRule[DenseVector, D] {

        override def sinS(v: DV): DV = breeze.numerics.sin(v)

        override def cosS(v: DV): DV = breeze.numerics.cos(v)

        override def tanS(v: DV): DV = breeze.numerics.tan(v)

        override def asinS(v: DV): DV = breeze.numerics.asin(v)

        override def acosS(v: DV): DV = breeze.numerics.acos(v)

        override def atanS(v: DV): DV = breeze.numerics.atan(v)

        override def sinhS(v: DV): DV = breeze.numerics.sinh(v)

        override def coshS(v: DV): DV = breeze.numerics.cosh(v)

        override def tanhS(v: DV): DV = breeze.numerics.tanh(v)

        override def logS(v: DV): DV = breeze.numerics.log(v)

        override def expS(v: DV): DV = breeze.numerics.exp(v)

        override def absS(v: DV): DV = breeze.numerics.abs(v)

        override def sqrtS(v: DV): DV = breeze.numerics.sqrt(v)

        override def sinM(v: D): D = breeze.numerics.sin(v)

        override def cosM(v: D): D = breeze.numerics.cos(v)

        override def tanM(v: D): D = breeze.numerics.tan(v)

        override def asinM(v: D): D = breeze.numerics.asin(v)

        override def acosM(v: D): D = breeze.numerics.acos(v)

        override def atanM(v: D): D = breeze.numerics.atan(v)

        override def sinhM(v: D): D = breeze.numerics.sinh(v)

        override def coshM(v: D): D = breeze.numerics.cosh(v)

        override def tanhM(v: D): D = breeze.numerics.tanh(v)

        override def lnM(v: D): D = breeze.numerics.log(v)

        override def expM(v: D): D = breeze.numerics.exp(v)

        override def absM(v: D): D = breeze.numerics.abs(v)

        override def sqrtM(v: D): D = breeze.numerics.sqrt(v)

        // TODO: fix this implicit argument error
        override def powSS(v: DV, p: DV): DV = breeze.numerics.pow(v, p)

        override def powSM(v: DV, p: D): DV = breeze.numerics.pow(v, p)

        override def powMS(v: D, p: DV): DV = breeze.numerics.pow(v, p)

        override def powMM(v: D, p: D): D = breeze.numerics.pow(v, p)

        override def dotSS(a: DV, b: DV): DV = DenseVector(a dot b) // FIXME

        override def dotSM(a: DV, b: D): DV = a *:* b

        override def dotMS(a: D, b: DV): DV = a *:* b

        override def dotMM(a: D, b: D): D = a *:* b
    }

    trait DenseMatrixValueRule extends ValueRule[DenseMatrix, D] {

        def adjustSize(left: DM, right: DM): (DM, DM) = {
            var ll = left
            var rr = right

            if (left.size < right.size) {
                ll = DenseMatrix.zeros[Double](right.rows, right.cols)
                ll(0, ::) := left.toDenseVector.t
            } else if (left.size > right.size) {
                rr = DenseMatrix.zeros(left.rows, left.cols)
                rr(0, ::) := right.toDenseVector.t
            }
            (ll, rr)
        }

        override def zeroM: D = 0.0

        override def zeroC(shape: DM): DM = DenseMatrix.zeros[D](shape.rows, shape.cols)

        override def oneM: D = 1.0

        override def oneC(shape: DM): DM = DenseMatrix.ones[D](shape.rows, shape.cols)

        override def addCC(l: DM, r: DM): DM = l + r

        override def mulCC(l: DM, r: DM): DM = {
            //            val (ll, rr) = adjustSize(l, r)
            //            ll *:* rr
            l *:* r
        }

        override def divCC(l: DM, r: DM): DM = l /:/ r

        override def addCM(l: DM, r: D): DM = l + r

        override def mulCM(l: DM, r: D): DM = l * r

        override def divCM(l: DM, r: D): DM = l / r

        override def addMC(l: D, r: DM): DM = r + l

        override def mulMC(l: D, r: DM): DM = r *:* l

        override def divMC(l: D, r: DM): DM = r.map(l / _)

        override def addMM(l: D, r: D): D = l + r

        override def mulMM(l: D, r: D): D = l * r

        override def divMM(l: D, r: D): D = l / r

        override def ltCC(l: DM, r: DM): DenseMatrix[Boolean] = l <:< r

        override def lteCC(l: DM, r: DM): DenseMatrix[Boolean] = l <:= r

        override def gtCC(l: DM, r: DM): DenseMatrix[Boolean] = l >:> r

        override def gteCC(l: DM, r: DM): DenseMatrix[Boolean] = l >:= r

        override def eqCC(l: DM, r: DM): DenseMatrix[Boolean] = l :== r

        override def ltCM(l: DM, r: D): DenseMatrix[Boolean] = l.map(_ < r)

        override def lteCM(l: DM, r: D): DenseMatrix[Boolean] = l.map(_ <= r)

        override def gtCM(l: DM, r: D): DenseMatrix[Boolean] = l.map(_ > r)

        override def gteCM(l: DM, r: D): DenseMatrix[Boolean] = l.map(_ >= r)

        override def eqCM(l: DM, r: D): DenseMatrix[Boolean] = l.map(_ == r)

        override def ltMC(l: D, r: DM): DenseMatrix[Boolean] = r.map(l < _)

        override def lteMC(l: D, r: DM): DenseMatrix[Boolean] = r.map(l <= _)

        override def gtMC(l: D, r: DM): DenseMatrix[Boolean] = r.map(l > _)

        override def gteMC(l: D, r: DM): DenseMatrix[Boolean] = r.map(l >= _)

        override def eqMC(l: D, r: DM): DenseMatrix[Boolean] = r.map(l == _)

        override def ltMM(l: D, r: D): Boolean = l < r

        override def lteMM(l: D, r: D): Boolean = l <= r

        override def gtMM(l: D, r: D): Boolean = l > r

        override def gteMM(l: D, r: D): Boolean = l >= r

        override def eqMM(l: D, r: D): Boolean = l == r

        override def posC(v: DM): DM = v

        override def negC(v: DM): DM = -v

        override def posM(v: D): D = v

        override def negM(v: D): D = -v

        override def transposeC(v: DM): DM = v.t

        override def transposeM(v: D): D = v

        override def closeCC(l: DM, r: DM, eps: D = 1e-4): DenseMatrix[Boolean] = {
            (breeze.numerics.abs(subCC(l, r)) <:= eps).map(b => b)
        }

        override def subCC(l: DM, r: DM): DM = l - r

        override def closeCM(l: DM, r: D, eps: D = 1e-4): DenseMatrix[Boolean] = {
            (breeze.numerics.abs(subCM(l, r)) <:= eps).map(b => b)
        }

        override def subCM(l: DM, r: D): DM = l - r

        override def closeMC(l: D, r: DM, eps: D = 1e-4): DenseMatrix[Boolean] = {
            (breeze.numerics.abs(subMC(l, r)) <:= eps).map(b => b)
        }

        override def subMC(l: D, r: DM): DM = r.map(l - _)

        override def closeMM(l: D, r: D, eps: D = 1e-4): Boolean = {
            breeze.numerics.abs(subMM(l, r)) <= eps
        }

        override def subMM(l: D, r: D): D = l - r

        override def whereCCC(cond: DenseMatrix[Boolean], a: DM, b: DM): DM = breeze.linalg.where(cond, a, b)

        override def whereCCM(cond: DenseMatrix[Boolean], a: DM, b: D): DM = breeze.linalg.where(cond, a, fillLike(b, cond))

        override def whereCMC(cond: DenseMatrix[Boolean], a: D, b: DM): DM = breeze.linalg.where(cond, fillLike(a, cond), b)

        private[this] def fillLike[A](value: D, ref: DenseMatrix[A]): DM = DenseMatrix.fill(ref.rows, ref.cols)(value)

        override def whereCMM(cond: DenseMatrix[Boolean], a: D, b: D): DM = breeze.linalg.where(cond, fillLike(a, cond), fillLike(b, cond))

        override def whereMCC(cond: Boolean, a: DM, b: DM): DM = if (cond) a else b

        override def whereMCM(cond: Boolean, a: DM, b: D): DM = if (cond) a else fillLike(b, a)

        override def whereMMC(cond: Boolean, a: D, b: DM): DM = if (cond) fillLike(a, b) else b

        override def whereMMM(cond: Boolean, a: D, b: D): D = if (cond) a else b
    }

    trait DenseMatrixMathRule extends MathRule[DenseMatrix, D] {

        override def sinS(v: DM): DM = breeze.numerics.sin(v)

        override def cosS(v: DM): DM = breeze.numerics.cos(v)

        override def tanS(v: DM): DM = breeze.numerics.tan(v)

        override def asinS(v: DM): DM = breeze.numerics.asin(v)

        override def acosS(v: DM): DM = breeze.numerics.acos(v)

        override def atanS(v: DM): DM = breeze.numerics.atan(v)

        override def sinhS(v: DM): DM = breeze.numerics.sinh(v)

        override def coshS(v: DM): DM = breeze.numerics.cosh(v)

        override def tanhS(v: DM): DM = breeze.numerics.tanh(v)

        override def logS(v: DM): DM = breeze.numerics.log(v)

        override def expS(v: DM): DM = breeze.numerics.exp(v)

        override def absS(v: DM): DM = breeze.numerics.abs(v)

        override def sqrtS(v: DM): DM = breeze.numerics.sqrt(v)

        override def sinM(v: D): D = breeze.numerics.sin(v)

        override def cosM(v: D): D = breeze.numerics.cos(v)

        override def tanM(v: D): D = breeze.numerics.tan(v)

        override def asinM(v: D): D = breeze.numerics.asin(v)

        override def acosM(v: D): D = breeze.numerics.acos(v)

        override def atanM(v: D): D = breeze.numerics.atan(v)

        override def sinhM(v: D): D = breeze.numerics.sinh(v)

        override def coshM(v: D): D = breeze.numerics.cosh(v)

        override def tanhM(v: D): D = breeze.numerics.tanh(v)

        override def lnM(v: D): D = breeze.numerics.log(v)

        override def expM(v: D): D = breeze.numerics.exp(v)

        override def absM(v: D): D = breeze.numerics.abs(v)

        override def sqrtM(v: D): D = breeze.numerics.sqrt(v)

        // TODO
        override def powSS(v: DM, p: DM): DM = breeze.numerics.pow(v, p)

        override def powSM(v: DM, p: D): DM = breeze.numerics.pow(v, p)

        override def powMS(v: D, p: DM): DM = breeze.numerics.pow(v, p)

        override def powMM(v: D, p: D): D = breeze.numerics.pow(v, p)

        override def dotSS(a: DM, b: DM): DM = a * b

        override def dotSM(a: DM, b: D): DM = a *:* b

        override def dotMS(a: D, b: DM): DM = a *:* b

        override def dotMM(a: D, b: D): D = a *:* b
    }

    class DenseVectorRule extends DenseVectorValueRule with DenseVectorMathRule

    class DenseMatrixRule extends DenseMatrixValueRule with DenseMatrixMathRule

    object Implicits {

        implicit val denseVectorRule = new DenseVectorRule
        implicit val denseMatrixRule = new DenseMatrixRule

        // Literal conversion for constructing computational tree
        implicit def fromByte(v: Byte): ScalarConst[DenseVector, D] = ScalarConst[DenseVector, D](v.toDouble)

        implicit def fromShort(v: Short): ScalarConst[DenseVector, D] = ScalarConst[DenseVector, D](v.toDouble)

        implicit def fromInt(v: Int): ScalarConst[DenseVector, D] = ScalarConst[DenseVector, D](v.toDouble)

        implicit def fromLong(v: Long): ScalarConst[DenseVector, D] = ScalarConst[DenseVector, D](v.toDouble)

        implicit def fromFloat(v: Float): ScalarConst[DenseVector, D] = ScalarConst[DenseVector, D](v.toDouble)

        implicit def fromDouble(v: Double): ScalarConst[DenseVector, D] = ScalarConst[DenseVector, D](v)
    }

}
