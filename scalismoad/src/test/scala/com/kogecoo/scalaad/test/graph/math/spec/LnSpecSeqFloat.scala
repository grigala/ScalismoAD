package com.kogecoo.scalaad.test.graph.math.spec

import ch.grigala.ad.graph.{Node, ln}
import ch.grigala.ad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import org.scalacheck.Properties


object LnSpecSeqFloat extends Properties("Ln - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGenWithValueRange(-1e15f, 1e15f)
    val valueGen = new SeqFloatValueGenWithValueRange(-1e15f, 1e15f)
    val expects = new LnSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("ln(a) apply") = seqFloatSpecGen.applyScalar()
    property("ln(a) apply") = seqFloatSpecGen.applyContainer()
    property("ln(a) apply") = seqFloatSpecGen.applyVar()

    property("ln(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("ln(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("ln(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("ln(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("ln(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("ln(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("ln(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("ln(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("ln(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("ln(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("ln(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("ln(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("ln(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class LnSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = ln(node)

    override def applyScalar(a: Float): Float = scala.math.log(a).toFloat

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.log(x.toDouble).toFloat }

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.log(x.toDouble).toFloat }

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => 1f / x }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => b / x }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => y / x }

    override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => 1f / x }

}
