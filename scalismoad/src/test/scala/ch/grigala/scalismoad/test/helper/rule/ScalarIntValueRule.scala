package ch.grigala.scalismoad.test.helper.rule

import ch.grigala.scalismoad.graph.Scalar
import ch.grigala.scalismoad.rule._


object ScalarIntValueRule {

    object Implicits {

        implicit val scalarIntValueRule = new ScalarIntValueRule
        implicit val scalarIntMathRule = new ScalarIntValueRule with ScalarIntMathRule

    }

}


class ScalarIntValueRule extends ValueRule[Scalar, Int] {

    override def zeroM: Int = 0

    override def zeroS(shape: Scalar[Int]): Scalar[Int] = Scalar(0)

    override def oneM: Int = 1

    override def oneS(shape: Scalar[Int]): Scalar[Int] = Scalar(1)

    override def addSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Int] = Scalar(l.data + r.data)

    override def subSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Int] = Scalar(l.data - r.data)

    override def mulSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Int] = Scalar(l.data * r.data)

    override def divSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Int] = Scalar(l.data / r.data)

    override def addSM(l: Scalar[Int], r: Int): Scalar[Int] = Scalar(l.data + r)

    override def subSM(l: Scalar[Int], r: Int): Scalar[Int] = Scalar(l.data - r)

    override def mulSM(l: Scalar[Int], r: Int): Scalar[Int] = Scalar(l.data * r)

    override def divSM(l: Scalar[Int], r: Int): Scalar[Int] = Scalar(l.data / r)

    override def addMS(l: Int, r: Scalar[Int]): Scalar[Int] = Scalar(l + r.data)

    override def subMS(l: Int, r: Scalar[Int]): Scalar[Int] = Scalar(l - r.data)

    override def mulMS(l: Int, r: Scalar[Int]): Scalar[Int] = Scalar(l * r.data)

    override def divMS(l: Int, r: Scalar[Int]): Scalar[Int] = Scalar(l / r.data)

    override def addMM(l: Int, r: Int): Int = l + r

    override def subMM(l: Int, r: Int): Int = l - r

    override def mulMM(l: Int, r: Int): Int = l * r

    override def divMM(l: Int, r: Int): Int = l / r

    override def ltSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data < r.data)

    override def lteSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data <= r.data)

    override def gtSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data > r.data)

    override def gteSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data >= r.data)

    override def eqSS(l: Scalar[Int], r: Scalar[Int]): Scalar[Boolean] = Scalar(l.data == r.data)

    override def ltSM(l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data < r)

    override def lteSM(l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data <= r)

    override def gtSM(l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data > r)

    override def gteSM(l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data >= r)

    override def eqSM(l: Scalar[Int], r: Int): Scalar[Boolean] = Scalar(l.data == r)

    override def ltMS(l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)

    override def lteMS(l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)

    override def gtMS(l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)

    override def gteMS(l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)

    override def eqMS(l: Int, r: Scalar[Int]): Scalar[Boolean] = Scalar(l < r.data)

    override def ltMM(l: Int, r: Int): Boolean = l < r

    override def lteMM(l: Int, r: Int): Boolean = l <= r

    override def gtMM(l: Int, r: Int): Boolean = l > r

    override def gteMM(l: Int, r: Int): Boolean = l >= r

    override def eqMM(l: Int, r: Int): Boolean = l == r

    override def posS(v: Scalar[Int]): Scalar[Int] = Scalar(+v.data)

    override def negS(v: Scalar[Int]): Scalar[Int] = Scalar(-v.data)

    override def posM(v: Int): Int = +v

    override def negM(v: Int): Int = -v

    override def transposeS(v: Scalar[Int]): Scalar[Int] = v

    override def transposeM(v: Int): Int = v

    override def closeSS(l: Scalar[Int], r: Scalar[Int], eps: Int = 0): Scalar[Boolean] = {
        Scalar(l.data == r.data)
    }

    override def closeSM(l: Scalar[Int], r: Int, eps: Int = 0): Scalar[Boolean] = {
        Scalar(l.data == r)
    }

    override def closeMS(l: Int, r: Scalar[Int], eps: Int = 0): Scalar[Boolean] = {
        Scalar(l == r.data)
    }

    override def closeMM(l: Int, r: Int, eps: Int = 0): Boolean = {
        l == r
    }

    override def whereSSS(cond: Scalar[Boolean], a: Scalar[Int], b: Scalar[Int]): Scalar[Int] = if (cond.data) a else b

    override def whereSSM(cond: Scalar[Boolean], a: Scalar[Int], b: Int): Scalar[Int] = if (cond.data) a else Scalar(b)

    override def whereSMS(cond: Scalar[Boolean], a: Int, b: Scalar[Int]): Scalar[Int] = if (cond.data) Scalar(a) else b

    override def whereSMM(cond: Scalar[Boolean], a: Int, b: Int): Scalar[Int] = if (cond.data) Scalar(a) else Scalar(b)

    override def whereMSS(cond: Boolean, a: Scalar[Int], b: Scalar[Int]): Scalar[Int] = if (cond) a else b

    override def whereMSM(cond: Boolean, a: Scalar[Int], b: Int): Scalar[Int] = if (cond) a else Scalar(b)

    override def whereMMS(cond: Boolean, a: Int, b: Scalar[Int]): Scalar[Int] = if (cond) Scalar(a) else b

    override def whereMMM(cond: Boolean, a: Int, b: Int): Int = if (cond) a else b

}

trait ScalarIntMathRule extends MathRule[Scalar, Int] {

    override def sinS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.sin(v.data.toDouble).toInt)

    override def cosS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.cos(v.data.toDouble).toInt)

    override def tanS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.tan(v.data.toDouble).toInt)

    override def asinS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.asin(v.data.toDouble).toInt)

    override def acosS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.acos(v.data.toDouble).toInt)

    override def atanS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.atan(v.data.toDouble).toInt)

    override def sinhS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.sinh(v.data.toDouble).toInt)

    override def coshS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.cosh(v.data.toDouble).toInt)

    override def tanhS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.tanh(v.data.toDouble).toInt)

    override def logS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.log(v.data.toDouble).toInt)

    override def expS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.exp(v.data.toDouble).toInt)

    override def absS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.abs(v.data.toDouble).toInt)

    override def sqrtS(v: Scalar[Int]): Scalar[Int] = Scalar(scala.math.sqrt(v.data.toDouble).toInt)

    override def sinM(v: Int): Int = scala.math.sin(v.toDouble).toInt

    override def cosM(v: Int): Int = scala.math.cos(v.toDouble).toInt

    override def tanM(v: Int): Int = scala.math.tan(v.toDouble).toInt

    override def asinM(v: Int): Int = scala.math.asin(v.toDouble).toInt

    override def acosM(v: Int): Int = scala.math.acos(v.toDouble).toInt

    override def atanM(v: Int): Int = scala.math.atan(v.toDouble).toInt

    override def sinhM(v: Int): Int = scala.math.sinh(v.toDouble).toInt

    override def coshM(v: Int): Int = scala.math.cosh(v.toDouble).toInt

    override def tanhM(v: Int): Int = scala.math.tanh(v.toDouble).toInt

    override def logM(v: Int): Int = scala.math.log(v.toDouble).toInt

    override def expM(v: Int): Int = scala.math.exp(v.toDouble).toInt

    override def absM(v: Int): Int = scala.math.abs(v)

    override def sqrtM(v: Int): Int = scala.math.sqrt(v.toDouble).toInt

    override def powSS(v: Scalar[Int], p: Scalar[Int]): Scalar[Int] = Scalar(scala.math.pow(v.data.toDouble, p.data.toDouble).toInt)

    override def powSM(v: Scalar[Int], p: Int): Scalar[Int] = Scalar(scala.math.pow(v.data.toDouble, p.toDouble).toInt)

    override def powMS(v: Int, p: Scalar[Int]): Scalar[Int] = Scalar(scala.math.pow(v.toDouble, p.data.toDouble).toInt)

    override def powMM(v: Int, p: Int): Int = scala.math.pow(v.toDouble, p.toDouble).toInt

    override def dotSS(a: Scalar[Int], b: Scalar[Int]): Scalar[Int] = Scalar(a.data * b.data)

    override def dotSM(a: Scalar[Int], b: Int): Scalar[Int] = Scalar(a.data * b)

    override def dotMS(a: Int, b: Scalar[Int]): Scalar[Int] = Scalar(a * b.data)

    override def dotMM(a: Int, b: Int): Int = a * b

}

