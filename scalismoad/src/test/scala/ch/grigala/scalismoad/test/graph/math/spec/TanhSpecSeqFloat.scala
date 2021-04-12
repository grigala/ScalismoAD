package ch.grigala.scalismoad.test.graph.math.spec

import ch.grigala.scalismoad.graph.{Node, tanh}
import ch.grigala.scalismoad.rule._
import ch.grigala.scalismoad.test.helper.gen.{SeqFloatNodeGenWithValueRange, SeqFloatValueGenWithValueRange}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatSoftCompareRule
import ch.grigala.scalismoad.test.helper.gen._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import ch.grigala.scalismoad.test.helper.specgen.UnaryOpExpectedBehaviorDef
import org.scalacheck.Properties


object TanhSpecSeqFloat extends Properties("Tanh - Seq[Float]") {

  implicit val compareRule = new SeqFloatSoftCompareRule

  val nodeGen = new SeqFloatNodeGenWithValueRange(-1e15f, 1e15f)
  val valueGen = new SeqFloatValueGenWithValueRange(-1e15f, 1e15f)
  val expects = new TanhSeqFloatExpectedBehavior

  val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

  property("tanh(a) apply") = seqFloatSpecGen.applyScalar()
  property("tanh(a) apply") = seqFloatSpecGen.applyContainer()
  property("tanh(a) apply") = seqFloatSpecGen.applyVar()

  property("tanh(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
  property("tanh(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
  property("tanh(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
  property("tanh(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

  property("tanh(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
  property("tanh(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
  property("tanh(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
  property("tanh(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
  property("tanh(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
  property("tanh(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

  property("tanh(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
  property("tanh(a) (container) grad") = seqFloatSpecGen.gradContainer()
  property("tanh(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class TanhSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

  override val zero: Float = 0f

  override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

  override def op(node: Node[Seq, Float]): Node[Seq, Float] = tanh(node)

  override def applyScalar(a: Float): Float = scala.math.tanh(a).toFloat

  override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.tanh(x.toDouble).toFloat }

  override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.tanh(x.toDouble).toFloat }

  override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x =>
    val tanhx = scala.math.tanh(x.toDouble).toFloat
    1f - tanhx * tanhx
  }

  override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x =>
    val tanhx = scala.math.tanh(x.toDouble).toFloat
    b * (1f - tanhx * tanhx)
  }

  override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) =>
    val tanhx = scala.math.tanh(x.toDouble).toFloat
    y * (1f - tanhx * tanhx)
  }

  override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x =>
    val tanhx = scala.math.tanh(x.toDouble).toFloat
    1f - tanhx * tanhx
  }

}
