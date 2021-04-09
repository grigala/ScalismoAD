package com.kogecoo.scalaad.test.helper.rule

import ch.grigala.ad.rule._

object SeqFloatValueRule {

    object Implicits {

        implicit val seqFloatValueRule = new SeqFloatValueRule
        implicit val seqFloatMathRule = new SeqFloatValueRule with SeqFloatMathRule

    }

}


class SeqFloatValueRule extends ValueRule[Seq, Float] {

    override def zeroM: Float = 0.0f

    override def zeroS(shape: Seq[Float]): Seq[Float] = Seq.fill[Float](shape.size)(0)

    override def oneM: Float = 1.0f

    override def oneS(shape: Seq[Float]): Seq[Float] = Seq.fill[Float](shape.size)(1)

    override def addSS(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case (a, b) => a + b }

    override def mulSS(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case (a, b) => a * b }

    override def divSS(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case (a, b) => a / b }

    override def addSM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ + r)

    override def mulSM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ * r)

    override def divSM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ / r)

    override def addMS(l: Float, r: Seq[Float]): Seq[Float] = r.map(l + _)

    override def mulMS(l: Float, r: Seq[Float]): Seq[Float] = r.map(l * _)

    override def divMS(l: Float, r: Seq[Float]): Seq[Float] = r.map(l / _)

    override def addMM(l: Float, r: Float): Float = l + r

    override def mulMM(l: Float, r: Float): Float = l * r

    override def divMM(l: Float, r: Float): Float = l / r

    override def ltSS(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a < b }

    override def lteSS(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a <= b }

    override def gtSS(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a > b }

    override def gteSS(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a >= b }

    override def eqSS(l: Seq[Float], r: Seq[Float]): Seq[Boolean] = l.zip(r).map { case (a, b) => a == b }

    override def ltSM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ < r)

    override def gtSM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ > r)

    override def gteSM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ >= r)

    override def eqSM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ == r)

    override def ltMS(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l < _)

    override def lteMS(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l <= _)

    override def gtMS(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l > _)

    override def gteMS(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l >= _)

    override def eqMS(l: Float, r: Seq[Float]): Seq[Boolean] = r.map(l == _)

    override def ltMM(l: Float, r: Float): Boolean = l < r

    override def gtMM(l: Float, r: Float): Boolean = l > r

    override def gteMM(l: Float, r: Float): Boolean = l >= r

    override def eqMM(l: Float, r: Float): Boolean = l == r

    override def posS(v: Seq[Float]): Seq[Float] = v.map(+_)

    override def negS(v: Seq[Float]): Seq[Float] = v.map(-_)

    override def posM(v: Float): Float = +v

    override def negM(v: Float): Float = -v

    override def transposeS(v: Seq[Float]): Seq[Float] = v

    override def transposeM(v: Float): Float = v

    override def closeSS(l: Seq[Float], r: Seq[Float], eps: Float = 1e-4f): Seq[Boolean] = {
        lteSM(subSS(l, r), eps)
    }

    override def subSS(l: Seq[Float], r: Seq[Float]): Seq[Float] = l.zip(r).map { case (a, b) => a - b }

    override def closeSM(l: Seq[Float], r: Float, eps: Float = 1e-4f): Seq[Boolean] = {
        lteSM(subSM(l, r), eps)
    }

    override def subSM(l: Seq[Float], r: Float): Seq[Float] = l.map(_ - r)

    override def closeMS(l: Float, r: Seq[Float], eps: Float = 1e-4f): Seq[Boolean] = {
        lteSM(subMS(l, r), eps)
    }

    override def lteSM(l: Seq[Float], r: Float): Seq[Boolean] = l.map(_ <= r)

    override def subMS(l: Float, r: Seq[Float]): Seq[Float] = r.map(l - _)

    override def closeMM(l: Float, r: Float, eps: Float = 1e-4f): Boolean = {
        lteMM(subMM(l, r), eps)
    }

    override def subMM(l: Float, r: Float): Float = l - r

    override def lteMM(l: Float, r: Float): Boolean = l <= r

    override def whereSSS(cond: Seq[Boolean], a: Seq[Float], b: Seq[Float]): Seq[Float] = {
        cond.zip(a.zip(b)).map { case (c, (x, y)) => if (c) x else y }
    }

    override def whereSSM(cond: Seq[Boolean], a: Seq[Float], b: Float): Seq[Float] = {
        cond.zip(a).map { case (c, x) => if (c) x else b }
    }

    override def whereSMS(cond: Seq[Boolean], a: Float, b: Seq[Float]): Seq[Float] = {
        cond.zip(b).map { case (c, y) => if (c) a else y }
    }

    override def whereSMM(cond: Seq[Boolean], a: Float, b: Float): Seq[Float] = {
        cond.map { case c => if (c) a else b }
    }

    override def whereMSS(cond: Boolean, a: Seq[Float], b: Seq[Float]): Seq[Float] = {
        if (cond) a else b
    }

    override def whereMSM(cond: Boolean, a: Seq[Float], b: Float): Seq[Float] = {
        if (cond) a else a.map(_ => b)
    }

    override def whereMMS(cond: Boolean, a: Float, b: Seq[Float]): Seq[Float] = {
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

    override def lnS(v: Seq[Float]): Seq[Float] = v.map(scala.math.log(_).toFloat)

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

