package com.kogecoo.scalaad.test.graph.op.spec

import com.kogecoo.scalaad.graph.{Mul, Node}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{BinaryOpExpectedBehaviorDef, BinaryOpSpec}
import org.scalacheck.Properties


object MulSpecSeqFloat extends Properties("Mul - Seq[Float]") {

  val nodeGen = new SeqFloatNodeGenWithValueRange()
  val valueGen = new SeqFloatValueGenWithValueRange()
  val expects = new MulSeqFloatExpectedBehavior
  implicit val compareRule = new SeqFloatSoftCompareRule

  val specGen = new BinaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

  val r = (x: Float) => !x.equals(Float.NaN) && !x.equals(Float.PositiveInfinity) && !x.equals(Float.NegativeInfinity)

  property("scalar * scalar       apply") = specGen.applyScalarScalar(r, r)
  property("scalar * container    apply") = specGen.applyScalarContainer(r, r)
  property("scalar * var          apply") = specGen.applyScalarVar(r, r)
  property("container * scalar    apply") = specGen.applyContainerScalar(r, r)
  property("container * container apply") = specGen.applyContainerContainer(r, r)
  property("container * var       apply") = specGen.applyContainerVar(r, r)
  property("var * scalar          apply") = specGen.applyVarScalar(r, r)
  property("var * container       apply") = specGen.applyVarContainer(r, r)
  property("var * var             apply") = specGen.applyVarVar(r, r)

  property("scalar * var          deriv w.r.t. right") = specGen.derivScalarVarWrtRight(r, r)
  property("scalar * var          deriv w.r.t. unknown") = specGen.derivScalarVarWrtUnknown(r, r)
  property("container * var       deriv w.r.t. right") = specGen.derivContainerVarWrtRight(r, r)
  property("container * var       deriv w.r.t. unknown") = specGen.derivContainerVarWrtUnknown(r, r)
  property("var * scalar          deriv w.r.t. left") = specGen.derivVarScalarWrtLeft(r, r)
  property("var * scalar          deriv w.r.t. unknown") = specGen.derivVarScalarWrtUnknown(r, r)
  property("var * container       deriv w.r.t. left") = specGen.derivVarContainerWrtLeft(r, r)
  property("var * container       deriv w.r.t. unknown") = specGen.derivVarContainerWrtUnknown(r, r)
  property("var * var             deriv w.r.t. left") = specGen.derivVarVarWrtLeft(r, r)
  property("var * var             deriv w.r.t. right") = specGen.derivVarVarWrtRight(r, r)
  property("var * var             deriv w.r.t. unknown") = specGen.derivVarVarWrtUnknown(r, r)
  property("var * var             deriv w.r.t. self") = specGen.derivVarVarWrtSelf(r)

  property("scalar * scalar       propagete value") = specGen.propagateScalarScalarWithNCValue(r, r, r)
  property("scalar * scalar       propagete container") = specGen.propagateScalarScalarWithCValue(r, r, r)
  property("scalar * container    propagete value") = specGen.propagateScalarContainerWithNCValue(r, r, r)
  property("scalar * container    propagete container") = specGen.propagateScalarContainerWithCValue(r, r, r)
  property("scalar * var          propagete value") = specGen.propagateScalarVarWithNCValue(r, r, r)
  property("scalar * var          propagete container") = specGen.propagateScalarVarWithCValue(r, r, r)
  property("container * scalar    propagete value") = specGen.propagateContainerScalarWithNCValue(r, r, r)
  property("container * scalar    propagete container") = specGen.propagateContainerScalarWithCValue(r, r, r)
  property("container * container propagete value") = specGen.propagateContainerContainerWithNCValue(r, r, r)
  property("container * container propagete container") = specGen.propagateContainerContainerWithCValue(r, r, r)
  property("container * var       propagete value") = specGen.propagateContainerVarWithNCValue(r, r, r)
  property("container * var       propagete container") = specGen.propagateContainerVarWithCValue(r, r, r)
  property("var * scalar          propagete value") = specGen.propagateVarScalarWithNCValue(r, r, r)
  property("var * scalar          propagete container") = specGen.propagateVarScalarWithCValue(r, r, r)
  property("var * container       propagete value") = specGen.propagateVarContainerWithNCValue(r, r, r)
  property("var * container       propagete container") = specGen.propagateVarContainerWithCValue(r, r, r)
  property("var * var             propagete value") = specGen.propagateVarVarWithNCValue(r, r, r)
  property("var * var             propagete container") = specGen.propagateVarVarWithCValue(r, r, r)

  property("scalar * scalar       grad") = specGen.gradScalarScalar(r, r)
  property("scalar * container    grad") = specGen.gradScalarContainer(r, r)
  property("scalar * var          grad") = specGen.gradScalarVar(r, r)
  property("container * scalar    grad") = specGen.gradContainerScalar(r, r)
  property("container * container grad") = specGen.gradContainerContainer(r, r)
  property("container * var       grad") = specGen.gradContainerVar(r, r)
  property("var * scalar          grad") = specGen.gradVarScalar(r, r)
  property("var * container       grad") = specGen.gradVarContainer(r, r)
  property("var * var             grad") = specGen.gradVarVar(r, r)

}


class MulSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends BinaryOpExpectedBehaviorDef[Seq, Float] {

  override val zero = 0f
  val one = 1f

  override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

  def one(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(1f)


  override def op(a: Node[Seq, Float], b: Node[Seq, Float]): Node[Seq, Float] = Mul(a, b)

  override def applyScalarScalar(a: Float, b: Float): Float = a * b

  override def applyScalarContainer(a: Float, b: Seq[Float]): Seq[Float] = b.map(a * _)

  override def applyScalarVar(a: Float, b: Seq[Float]): Seq[Float] = b.map(a * _)

  override def applyContainerScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(_ * b)

  override def applyContainerContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x * y }

  override def applyContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x * y }

  override def applyVarScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(_ * b)

  override def applyVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x * y }

  override def applyVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x * y }


  override def derivScalarVarWrtRight(a: Float, b: Seq[Float]): Seq[Float] = Seq.fill(b.size)(a)

  override def derivContainerVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = a

  override def derivVarContainerWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = b

  override def derivVarScalarWrtLeft(a: Seq[Float], b: Float): Seq[Float] = Seq.fill(a.size)(b)

  override def derivVarVarWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = b

  override def derivVarVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = a

  override def derivVarVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map(2f * _)


  override def propagateScalarVarWithNCValue(a: Float, b: Seq[Float], c: Float): Seq[Float] = Seq.fill(b.size)(a * c)

  override def propagateScalarVarWithCValue(a: Float, b: Seq[Float], c: Seq[Float]): Seq[Float] = Seq.fill(b.size)(a).zip(c).map { case (y, z) => y * z }

  override def propagateContainerVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.map(_ * c)

  override def propagateContainerVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(c).map { case (x, z) => x * z }

  override def propagateVarScalarWithCValue(a: Seq[Float], b: Float, c: Seq[Float]): Seq[Float] = c.map(b * _)

  override def propagateVarScalarWithNCValue(a: Seq[Float], b: Float, c: Float): Seq[Float] = Seq.fill(a.size)(b * c)

  override def propagateVarContainerWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = b.map(_ * c)

  override def propagateVarContainerWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = b.zip(c).map { case (y, z) => y * z }

  override def propagateVarVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.zip(b).map { case (x, y) => x * c + y * c }

  override def propagateVarVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c) map { case ((x, y), z) => x * z + y * z }


  override def gradScalarVar(a: Float, b: Seq[Float]): Seq[Float] = Seq.fill(b.size)(a)

  override def gradContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a

  override def gradVarScalar(a: Seq[Float], b: Float): Seq[Float] = Seq.fill(a.size)(b)

  override def gradVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = b

  override def gradVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x + y }

}
