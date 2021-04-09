package com.kogecoo.scalaad.test.graph.math.spec

import ch.grigala.ad.graph.{Node, sin}
import ch.grigala.ad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import org.scalacheck.Properties


object SinSpecSeqFloat extends Properties("Sin - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGen
    val valueGen = new SeqFloatValueGen
    val expects = new SinSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)


    property("sin(a) apply") = seqFloatSpecGen.applyScalar()
    property("sin(a) apply") = seqFloatSpecGen.applyContainer()
    property("sin(a) apply") = seqFloatSpecGen.applyVar()

    property("sin(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("sin(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("sin(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("sin(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("sin(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("sin(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("sin(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("sin(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("sin(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("sin(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("sin(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("sin(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("sin(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class SinSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = sin(node)

    override def applyScalar(a: Float): Float = scala.math.sin(a).toFloat

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.sin(x.toDouble).toFloat }

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.sin(x.toDouble).toFloat }

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.cos(x.toDouble).toFloat }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => b * scala.math.cos(x.toDouble).toFloat }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => y * scala.math.cos(x.toDouble).toFloat }

    override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.cos(x.toDouble).toFloat }

}
