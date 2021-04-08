package com.kogecoo.scalaad.test.graph.math.spec

import com.kogecoo.scalaad.graph.{Node, asin}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import org.scalacheck.Properties


object AsinSpecSeqFloat extends Properties("Asin - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGenWithValueRange(-1f, 1f)
    val valueGen = new SeqFloatValueGenWithValueRange(-1f, 1f)
    val expects = new AsinSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("asin(a) apply") = seqFloatSpecGen.applyScalar()
    property("asin(a) apply") = seqFloatSpecGen.applyContainer()
    property("asin(a) apply") = seqFloatSpecGen.applyVar()

    property("asin(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("asin(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("asin(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("asin(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("asin(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("asin(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("asin(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("asin(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("asin(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("asin(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("asin(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("asin(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("asin(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class AsinSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = asin(node)

    override def applyScalar(a: Float): Float = scala.math.asin(a).toFloat

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.asin(x.toDouble).toFloat }

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.asin(x.toDouble).toFloat }

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => 1f / scala.math.sqrt(1f - x * x).toFloat }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => b / scala.math.sqrt(1f - x * x).toFloat }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) =>
        y / scala.math.sqrt(1f - x * x).toFloat
    }

    override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => 1f / scala.math.sqrt(1f - x * x).toFloat }

}
