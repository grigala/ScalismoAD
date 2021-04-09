package com.kogecoo.scalaad.test.graph.math.spec

import ch.grigala.ad.graph.{Node, sinh}
import ch.grigala.ad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import org.scalacheck.Properties


object SinhSpecSeqFloat extends Properties("Sinh - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGenWithValueRange(-25f, 25f)
    val valueGen = new SeqFloatValueGenWithValueRange(-25f, 25f)
    val expects = new SinhSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("sinh(a) apply") = seqFloatSpecGen.applyScalar()
    property("sinh(a) apply") = seqFloatSpecGen.applyContainer()
    property("sinh(a) apply") = seqFloatSpecGen.applyVar()

    property("sinh(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("sinh(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("sinh(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("sinh(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("sinh(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("sinh(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("sinh(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("sinh(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("sinh(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("sinh(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("sinh(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("sinh(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("sinh(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class SinhSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = sinh(node)

    override def applyScalar(a: Float): Float = scala.math.sinh(a).toFloat

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.sinh(x.toDouble).toFloat }

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.sinh(x.toDouble).toFloat }

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.cosh(x.toDouble).toFloat }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => b * scala.math.cosh(x.toDouble).toFloat }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => y * scala.math.cosh(x.toDouble).toFloat }

    override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.cosh(x.toDouble).toFloat }

}
