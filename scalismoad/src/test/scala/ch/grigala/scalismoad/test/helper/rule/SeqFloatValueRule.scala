package ch.grigala.scalismoad.test.helper.rule

import ch.grigala.scalismoad.rule._

object SeqFloatValueRule {

    object Implicits {

        implicit val seqFloatValueRule = new SeqFloatValueRule
        implicit val seqFloatMathRule = new SeqFloatValueRule with SeqFloatMathRule

    }

}


class SeqFloatValueRule extends ValueRule[Seq, Float] {

    override def zeroM: Float = 0.0f

    override def zeroC(shape: Seq[Float]): Seq[Float] = Seq.fill[Float](shape.size)(0)

    override def oneM: Float = 1.0f

    override def oneC(shape: Seq[Float]): Seq[Float] = Seq.fill[Float](shape.size)(1)

    override def addCC(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case (a, b) => a + b }

    override def mulCC(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case (a, b) => a * b }

    override def divCC(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case (a, b) => a / b }

    override def addCM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ + r)

    override def mulCM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ * r)

    override def divCM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ / r)

    override def addMC(l: Float, r: Seq[Float]): Seq[Float] = r.map(l + _)

    override def mulMC(l: Float, r: Seq[Float]): Seq[Float] = r.map(l * _)

    override def divMC(l: Float, r: Seq[Float]): Seq[Float] = r.map(l / _)

    override def addMM(l: Float, r: Float): Float = l + r

    override def mulMM(l: Float, r: Float): Float = l * r

    override def divMM(l: Float, r: Float): Float = l / r

    override def ltCC(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a < b }

    override def lteCC(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a <= b }

    override def gtCC(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a > b }

    override def gteCC(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a >= b }

    override def eqCC(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a == b }

    override def ltCM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ < r)

    override def gtCM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ > r)

    override def gteCM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ >= r)

    override def eqCM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ == r)

    override def ltMC(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l < _)

    override def lteMC(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l <= _)

    override def gtMC(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l > _)

    override def gteMC(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l >= _)

    override def eqMC(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l == _)

    override def ltMM(l: Float, r: Float): Boolean = l < r

    override def gtMM(l: Float, r: Float): Boolean = l > r

    override def gteMM(l: Float, r: Float): Boolean = l >= r

    override def eqMM(l: Float, r: Float): Boolean = l == r

    override def posC(v: Seq[Float]): Seq[Float] = v.map(+_)

    override def negC(v: Seq[Float]): Seq[Float] = v.map(-_)

    override def posM(v: Float): Float = +v

    override def negM(v: Float): Float = -v

    override def transposeC(v: Seq[Float]): Seq[Float] = v

    override def transposeM(v: Float): Float = v

    override def closeCC(l: Seq[Float], r: Seq[Float], eps: Float = 1e-4f): Seq[Boolean] = {
        lteCM(subCC(l, r), eps)
    }

    override def subCC(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case (a, b) => a - b }

    override def closeCM(l: Seq[Float], r: Float, eps: Float = 1e-4f): Seq[Boolean] = {
        lteCM(subCM(l, r), eps)
    }

    override def subCM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ - r)

    override def closeMC(l: Float, r: Seq[Float], eps: Float = 1e-4f): Seq[Boolean] = {
        lteCM(subMC(l, r), eps)
    }

    override def lteCM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ <= r)

    override def subMC(l: Float, r: Seq[Float]): Seq[Float] = r.map(l - _)

    override def closeMM(l: Float, r: Float, eps: Float = 1e-4f): Boolean = {
        lteMM(subMM(l, r), eps)
    }

    override def subMM(l: Float, r: Float): Float = l - r

    override def lteMM(l: Float, r: Float): Boolean = l <= r

    override def whereCCC(cond: Seq[Boolean], a: Seq[Float], b: Seq[Float]): Seq[Float] = {
        cond.zip(a.zip(b)).map { case (c, (x, y)) => if (c) x else y }
    }

    override def whereCCM(cond: Seq[Boolean], a: Seq[Float], b: Float): Seq[Float] = {
        cond.zip(a).map { case (c, x) => if (c) x else b }
    }

    override def whereCMC(cond: Seq[Boolean], a: Float, b: Seq[Float]): Seq[Float] = {
        cond.zip(b).map { case (c, y) => if (c) a else y }
    }

    override def whereCMM(cond: Seq[Boolean], a: Float, b: Float): Seq[Float] = {
        cond.map { case c => if (c) a else b }
    }

    override def whereMCC(cond: Boolean, a: Seq[Float], b: Seq[Float]): Seq[Float] = {
        if (cond) a else b
    }

    override def whereMCM(cond: Boolean, a: Seq[Float], b: Float): Seq[Float] = {
        if (cond) a else a.map(_ => b)
    }

    override def whereMMC(cond: Boolean, a: Float, b: Seq[Float]): Seq[Float] = {
        if (cond) b.map(_ => a) else b
    }

    override def whereMMM(cond: Boolean, a: Float, b: Float): Float = {
        if (cond) a else b
    }

}

trait SeqFloatMathRule extends MathRule[Seq, Float] {

    override def sinS(v: Seq[Float]): Seq[Float] = v.map(scala.math.sin(_).toFloat)

    override def cosS(v: Seq[Float]): Seq[Float] = v.map(scala.math.cos(_).toFloat)

    override def tanS(v: Seq[Float]): Seq[Float] = v.map(scala.math.tan(_).toFloat)

    override def asinS(v: Seq[Float]): Seq[Float] = v.map(scala.math.asin(_).toFloat)

    override def acosS(v: Seq[Float]): Seq[Float] = v.map(scala.math.acos(_).toFloat)

    override def atanS(v: Seq[Float]): Seq[Float] = v.map(scala.math.atan(_).toFloat)

    override def sinhS(v: Seq[Float]): Seq[Float] = v.map(scala.math.sinh(_).toFloat)

    override def coshS(v: Seq[Float]): Seq[Float] = v.map(scala.math.cosh(_).toFloat)

    override def tanhS(v: Seq[Float]): Seq[Float] = v.map(scala.math.tanh(_).toFloat)

    override def logS(v: Seq[Float]): Seq[Float] = v.map(scala.math.log(_).toFloat)

    override def expS(v: Seq[Float]): Seq[Float] = v.map(scala.math.exp(_).toFloat)

    override def absS(v: Seq[Float]): Seq[Float] = v.map(scala.math.abs(_))

    override def sqrtS(v: Seq[Float]): Seq[Float] = v.map(scala.math.sqrt(_).toFloat)

    override def sinM(v: Float): Float = scala.math.sin(v.toDouble).toFloat

    override def cosM(v: Float): Float = scala.math.cos(v.toDouble).toFloat

    override def tanM(v: Float): Float = scala.math.tan(v.toDouble).toFloat

    override def asinM(v: Float): Float = scala.math.asin(v.toDouble).toFloat

    override def acosM(v: Float): Float = scala.math.acos(v.toDouble).toFloat

    override def atanM(v: Float): Float = scala.math.atan(v.toDouble).toFloat

    override def sinhM(v: Float): Float = scala.math.sinh(v.toDouble).toFloat

    override def coshM(v: Float): Float = scala.math.cosh(v.toDouble).toFloat

    override def tanhM(v: Float): Float = scala.math.tanh(v.toDouble).toFloat

    override def lnM(v: Float): Float = scala.math.log(v.toDouble).toFloat

    override def expM(v: Float): Float = scala.math.exp(v.toDouble).toFloat

    override def absM(v: Float): Float = scala.math.abs(v)

    override def sqrtM(v: Float): Float = scala.math.sqrt(v.toDouble).toFloat

    override def powSS(v: Seq[Float], p: Seq[Float]): Seq[Float] = v.zip(p).map { case (a, b) => scala.math.pow(a, b).toFloat }

    override def powSM(v: Seq[Float], p: Float): Seq[Float] = v.map { a => scala.math.pow(a, p).toFloat }

    override def powMS(v: Float, p: Seq[Float]): Seq[Float] = p.map { b => scala.math.pow(v, b).toFloat }

    override def powMM(v: Float, p: Float): Float = scala.math.pow(v, p).toFloat

    override def dotSS(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(a.zip(b).map({ case (x, y) => x * y }).fold(0f)(_ + _))

    override def dotSM(a: Seq[Float], b: Float): Seq[Float] = Seq(a.map(_ * b).fold(0f)(_ + _))

    override def dotMS(a: Float, b: Seq[Float]): Seq[Float] = Seq(b.map(a * _).fold(0f)(_ + _))

    override def dotMM(a: Float, b: Float): Float = a * b

}

