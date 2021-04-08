package com.kogecoo.scalaad.test.graph.math.spec

import com.kogecoo.scalaad.graph.{Node, cos}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import org.scalacheck.Properties




object CosSpecSeqFloat extends Properties("Cos - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGen
    val valueGen = new SeqFloatValueGen
    val expects = new CosSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("cos(a) apply") = seqFloatSpecGen.applyScalar()
    property("cos(a) apply") = seqFloatSpecGen.applyContainer()
    property("cos(a) apply") = seqFloatSpecGen.applyVar()

    property("cos(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("cos(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("cos(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("cos(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("cos(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("cos(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("cos(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("cos(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("cos(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("cos(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("cos(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("cos(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("cos(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class CosSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = cos(node)

    override def applyScalar(a: Float): Float = scala.math.cos(a).toFloat

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.cos(x.toDouble).toFloat }

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.cos(x.toDouble).toFloat }

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => -scala.math.sin(x.toDouble).toFloat }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => -b * scala.math.sin(x.toDouble).toFloat }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => -y * scala.math.sin(x.toDouble).toFloat }

    override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => -scala.math.sin(x.toDouble).toFloat }

}
