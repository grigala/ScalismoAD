package ch.grigala.scalismoad.rule

import ch.grigala.scalismoad.graph.{Scalar, ScalarConst}

import scala.language.implicitConversions

object DoubleRule {

    trait DoubleValueRule extends ValueRule[Scalar, Double] {

        override def zeroM: Double = 0.0

        override def zeroC(shape: Scalar[Double]): Scalar[Double] = Scalar(0.0)

        override def oneM: Double = 1.0

        override def oneC(shape: Scalar[Double]): Scalar[Double] = Scalar(1.0)

        override def addCC(l: Scalar[Double], r: Scalar[Double]): Scalar[Double] = Scalar(l.data + r.data)

        override def subCC(l: Scalar[Double], r: Scalar[Double]): Scalar[Double] = Scalar(l.data - r.data)

        override def mulCC(l: Scalar[Double], r: Scalar[Double]): Scalar[Double] = Scalar(l.data * r.data)

        override def divCC(l: Scalar[Double], r: Scalar[Double]): Scalar[Double] = Scalar(l.data / r.data)

        override def addCM(l: Scalar[Double], r: Double): Scalar[Double] = Scalar(l.data + r)

        override def subCM(l: Scalar[Double], r: Double): Scalar[Double] = Scalar(l.data - r)

        override def mulCM(l: Scalar[Double], r: Double): Scalar[Double] = Scalar(l.data * r)

        override def divCM(l: Scalar[Double], r: Double): Scalar[Double] = Scalar(l.data / r)

        override def addMC(l: Double, r: Scalar[Double]): Scalar[Double] = Scalar(l + r.data)

        override def subMC(l: Double, r: Scalar[Double]): Scalar[Double] = Scalar(l - r.data)

        override def mulMC(l: Double, r: Scalar[Double]): Scalar[Double] = Scalar(l * r.data)

        override def divMC(l: Double, r: Scalar[Double]): Scalar[Double] = Scalar(l / r.data)

        override def addMM(l: Double, r: Double): Double = l + r

        override def subMM(l: Double, r: Double): Double = l - r

        override def mulMM(l: Double, r: Double): Double = l * r

        override def divMM(l: Double, r: Double): Double = l / r

        override def ltCC(l: Scalar[Double], r: Scalar[Double]): Scalar[Boolean] = Scalar(l.data < r.data)

        override def lteCC(l: Scalar[Double], r: Scalar[Double]): Scalar[Boolean] = Scalar(l.data <= r.data)

        override def gtCC(l: Scalar[Double], r: Scalar[Double]): Scalar[Boolean] = Scalar(l.data > r.data)

        override def gteCC(l: Scalar[Double], r: Scalar[Double]): Scalar[Boolean] = Scalar(l.data >= r.data)

        override def eqCC(l: Scalar[Double], r: Scalar[Double]): Scalar[Boolean] = Scalar(l.data == r.data)

        override def ltCM(l: Scalar[Double], r: Double): Scalar[Boolean] = Scalar(l.data < r)

        override def lteCM(l: Scalar[Double], r: Double): Scalar[Boolean] = Scalar(l.data <= r)

        override def gtCM(l: Scalar[Double], r: Double): Scalar[Boolean] = Scalar(l.data > r)

        override def gteCM(l: Scalar[Double], r: Double): Scalar[Boolean] = Scalar(l.data >= r)

        override def eqCM(l: Scalar[Double], r: Double): Scalar[Boolean] = Scalar(l.data == r)

        override def ltMC(l: Double, r: Scalar[Double]): Scalar[Boolean] = Scalar(l < r.data)

        override def lteMC(l: Double, r: Scalar[Double]): Scalar[Boolean] = Scalar(l <= r.data)

        override def gtMC(l: Double, r: Scalar[Double]): Scalar[Boolean] = Scalar(l > r.data)

        override def gteMC(l: Double, r: Scalar[Double]): Scalar[Boolean] = Scalar(l >= r.data)

        override def eqMC(l: Double, r: Scalar[Double]): Scalar[Boolean] = Scalar(l == r.data)

        override def ltMM(l: Double, r: Double): Boolean = l < r

        override def lteMM(l: Double, r: Double): Boolean = l <= r

        override def gtMM(l: Double, r: Double): Boolean = l > r

        override def gteMM(l: Double, r: Double): Boolean = l >= r

        override def eqMM(l: Double, r: Double): Boolean = l == r

        override def posC(v: Scalar[Double]): Scalar[Double] = Scalar(+v.data)

        override def negC(v: Scalar[Double]): Scalar[Double] = Scalar(-v.data)

        override def posM(v: Double): Double = +v

        override def negM(v: Double): Double = -v

        override def transposeC(v: Scalar[Double]): Scalar[Double] = v

        override def transposeM(v: Double): Double = v

        override def closeCC(l: Scalar[Double], r: Scalar[Double], eps: Double = 1e-4): Scalar[Boolean] = {
            Scalar[Boolean](scala.math.abs(l.data - r.data) <= eps)
        }

        override def closeCM(l: Scalar[Double], r: Double, eps: Double = 1e-4): Scalar[Boolean] = {
            Scalar[Boolean](scala.math.abs(l.data - r) <= eps)
        }

        override def closeMC(l: Double, r: Scalar[Double], eps: Double = 1e-4): Scalar[Boolean] = {
            Scalar[Boolean](scala.math.abs(l - r.data) <= eps)
        }

        override def closeMM(l: Double, r: Double, eps: Double = 1e-4): Boolean = {
            scala.math.abs(l - r) <= eps
        }

        override def whereCCC(cond: Scalar[Boolean], a: Scalar[Double], b: Scalar[Double]): Scalar[Double] = if (cond.data) a else b

        override def whereCCM(cond: Scalar[Boolean], a: Scalar[Double], b: Double): Scalar[Double] = if (cond.data) a else Scalar(b)

        override def whereCMC(cond: Scalar[Boolean], a: Double, b: Scalar[Double]): Scalar[Double] = if (cond.data) Scalar(a) else b

        override def whereCMM(cond: Scalar[Boolean], a: Double, b: Double): Scalar[Double] = if (cond.data) Scalar(a) else Scalar(b)

        override def whereMCC(cond: Boolean, a: Scalar[Double], b: Scalar[Double]): Scalar[Double] = if (cond) a else b

        override def whereMCM(cond: Boolean, a: Scalar[Double], b: Double): Scalar[Double] = if (cond) a else Scalar(b)

        override def whereMMC(cond: Boolean, a: Double, b: Scalar[Double]): Scalar[Double] = if (cond) Scalar(a) else b

        override def whereMMM(cond: Boolean, a: Double, b: Double): Double = if (cond) a else b

    }

    trait DoubleMathRule extends MathRule[Scalar, Double] {

        override def sinS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.sin(v.data))

        override def cosS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.cos(v.data))

        override def tanS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.tan(v.data))

        override def asinS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.asin(v.data))

        override def acosS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.acos(v.data))

        override def atanS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.atan(v.data))

        override def sinhS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.sinh(v.data))

        override def coshS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.cosh(v.data))

        override def tanhS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.tanh(v.data))

        override def logS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.log(v.data))

        override def expS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.exp(v.data))

        override def absS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.abs(v.data))

        override def sqrtS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.sqrt(v.data))

        override def sinM(v: Double): Double = scala.math.sin(v)

        override def cosM(v: Double): Double = scala.math.cos(v)

        override def tanM(v: Double): Double = scala.math.tan(v)

        override def asinM(v: Double): Double = scala.math.asin(v)

        override def acosM(v: Double): Double = scala.math.acos(v)

        override def atanM(v: Double): Double = scala.math.atan(v)

        override def sinhM(v: Double): Double = scala.math.sinh(v)

        override def coshM(v: Double): Double = scala.math.cosh(v)

        override def tanhM(v: Double): Double = scala.math.tanh(v)

        override def lnM(v: Double): Double = scala.math.log(v)

        override def expM(v: Double): Double = scala.math.exp(v)

        override def absM(v: Double): Double = scala.math.abs(v)

        override def sqrtM(v: Double): Double = scala.math.sqrt(v)

        override def powSS(v: Scalar[Double], p: Scalar[Double]): Scalar[Double] = Scalar(scala.math.pow(v.data, p.data))

        override def powSM(v: Scalar[Double], p: Double): Scalar[Double] = Scalar(scala.math.pow(v.data, p))

        override def powMS(v: Double, p: Scalar[Double]): Scalar[Double] = Scalar(scala.math.pow(v, p.data))

        override def powMM(v: Double, p: Double): Double = scala.math.pow(v, p)

        override def dotSS(a: Scalar[Double], b: Scalar[Double]): Scalar[Double] = Scalar(a.data * b.data)

        override def dotSM(a: Scalar[Double], b: Double): Scalar[Double] = Scalar(a.data * b)

        override def dotMS(a: Double, b: Scalar[Double]): Scalar[Double] = Scalar(a * b.data)

        override def dotMM(a: Double, b: Double): Double = a * b
    }

    class DoubleRule extends DoubleValueRule with DoubleMathRule

    class DoubleWrapperRule extends ValueWrapperRule[Double, Scalar, Double] {
        override def toWrapper(src: Double): Scalar[Double] = Scalar(src)

        override def toWrappee(data: Scalar[_]): Double = data.asInstanceOf[Double]
    }

    object Implicits {

        implicit val doubleRule: DoubleRule = new DoubleRule
        implicit val doubleWrapperRule: DoubleWrapperRule = new DoubleWrapperRule

        // Literal conversion for constructing computational tree
        implicit def fromByte(v: Byte): ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)

        implicit def fromShort(v: Short): ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)

        implicit def fromInt(v: Int): ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)

        implicit def fromLong(v: Long): ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)

        implicit def fromFloat(v: Float): ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)

        implicit def fromDouble(v: Double): ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v)
    }

}
