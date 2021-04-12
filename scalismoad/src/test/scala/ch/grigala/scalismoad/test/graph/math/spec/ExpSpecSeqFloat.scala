package ch.grigala.scalismoad.test.graph.math.spec

import ch.grigala.scalismoad.graph.{Node, exp}
import ch.grigala.scalismoad.rule._
import ch.grigala.scalismoad.test.helper.gen.{SeqFloatNodeGenWithValueRange, SeqFloatValueGenWithValueRange}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatSoftCompareRule
import ch.grigala.scalismoad.test.helper.gen._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import ch.grigala.scalismoad.test.helper.specgen.UnaryOpExpectedBehaviorDef
import org.scalacheck.Properties


object ExpSpecSeqFloat extends Properties("Exp - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGenWithValueRange(-30f, 30f)
    val valueGen = new SeqFloatValueGenWithValueRange(-30f, 30f)
    val expects = new ExpSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("exp(a) apply") = seqFloatSpecGen.applyScalar()
    property("exp(a) apply") = seqFloatSpecGen.applyContainer()
    property("exp(a) apply") = seqFloatSpecGen.applyVar()

    property("exp(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("exp(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("exp(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("exp(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("exp(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("exp(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("exp(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("exp(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("exp(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("exp(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("exp(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("exp(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("exp(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class ExpSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = exp(node)

    override def applyScalar(a: Float): Float = scala.math.exp(a).toFloat

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.exp(x.toDouble).toFloat }

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.exp(x.toDouble).toFloat }

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.exp(x).toFloat }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => b * scala.math.exp(x.toDouble).toFloat }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => y * scala.math.exp(x.toDouble).toFloat }

    override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.exp(x.toDouble).toFloat }

}
