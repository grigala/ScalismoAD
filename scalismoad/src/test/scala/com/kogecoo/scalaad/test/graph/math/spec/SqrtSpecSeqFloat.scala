package com.kogecoo.scalaad.test.graph.math.spec

import ch.grigala.ad.graph.{Node, sqrt}
import ch.grigala.ad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import org.scalacheck.Properties


object SqrtSpecSeqFloat extends Properties("Sqrt - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGenWithValueRange(0f, 1e30f)
    val valueGen = new SeqFloatValueGenWithValueRange(0f, 1e30f)
    val expects = new SqrtSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("sqrt(a) apply") = seqFloatSpecGen.applyScalar()
    property("sqrt(a) apply") = seqFloatSpecGen.applyContainer()
    property("sqrt(a) apply") = seqFloatSpecGen.applyVar()

    property("sqrt(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("sqrt(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("sqrt(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("sqrt(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("sqrt(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("sqrt(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("sqrt(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("sqrt(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("sqrt(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("sqrt(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("sqrt(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("sqrt(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("sqrt(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class SqrtSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = sqrt(node)

    override def applyScalar(a: Float): Float = scala.math.sqrt(a.toDouble).toFloat

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.sqrt(x.toDouble).toFloat }

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.sqrt(x.toDouble).toFloat }

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => 1f / (2 * scala.math.sqrt(x).toFloat) }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => b / (2 * scala.math.sqrt(x).toFloat) }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) =>
        y / (2 * scala.math.sqrt(x).toFloat)
    }

    override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => 1f / (2 * scala.math.sqrt(x).toFloat) }

}
