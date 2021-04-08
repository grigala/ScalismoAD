package com.kogecoo.scalaad.test.graph.op.spec

import com.kogecoo.scalaad.graph.{Div, Node}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{BinaryOpExpectedBehaviorDef, BinaryOpSpec}
import org.scalacheck.Properties


object DivSpecSeqFloat extends Properties("Div - Seq[Float]") {

  val nodeGen = new SeqFloatNodeGenWithValueRange(-1e15f, 1e15f)
  val valueGen = new SeqFloatValueGenWithValueRange(-1e15f, 1e15f)
  val expects = new DivSeqFloatExpectedBehavior
  implicit val compareRule = new SeqFloatSoftCompareRule

  val specGen = new BinaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

  val rExceptNaN = (x: Float) => !x.equals(Float.NaN)
  val rExceptInf = (x: Float) => !x.equals(Float.PositiveInfinity) && !x.equals(Float.NegativeInfinity)
  val rNonZero = (x: Float) => x != 0.0f
  val rNumerator = (x: Float) => rExceptNaN(x) && rExceptInf(x)
  val rDenominator = (x: Float) => rExceptNaN(x) && rExceptInf(x) && rNonZero(x)


  property("scalar / scalar       apply") = specGen.applyScalarScalar(rNumerator, rDenominator)
  property("scalar / container    apply") = specGen.applyScalarContainer(rNumerator, rDenominator)
  property("scalar / var          apply") = specGen.applyScalarVar(rNumerator, rDenominator)
  property("container / scalar    apply") = specGen.applyContainerScalar(rNumerator, rDenominator)
  property("container / container apply") = specGen.applyContainerContainer(rNumerator, rDenominator)
  property("container / var       apply") = specGen.applyContainerVar(rNumerator, rDenominator)
  property("var / scalar          apply") = specGen.applyVarScalar(rNumerator, rDenominator)
  property("var / container       apply") = specGen.applyVarContainer(rNumerator, rDenominator)
  property("var / var             apply") = specGen.applyVarVar(rNumerator, rDenominator)

  property("scalar / var          deriv w.r.t. right") = specGen.derivScalarVarWrtRight(rNumerator, rDenominator)
  property("scalar / var          deriv w.r.t. unknown") = specGen.derivScalarVarWrtUnknown(rNumerator, rDenominator)
  property("container / var       deriv w.r.t. right") = specGen.derivContainerVarWrtRight(rNumerator, rDenominator)
  property("container / var       deriv w.r.t. unknown") = specGen.derivContainerVarWrtUnknown(rNumerator, rDenominator)
  property("var / scalar          deriv w.r.t. left") = specGen.derivVarScalarWrtLeft(rNumerator, rDenominator)
  property("var / scalar          deriv w.r.t. unknown") = specGen.derivVarScalarWrtUnknown(rNumerator, rDenominator)
  property("var / container       deriv w.r.t. left") = specGen.derivVarContainerWrtLeft(rNumerator, rDenominator)
  property("var / container       deriv w.r.t. unknown") = specGen.derivVarContainerWrtUnknown(rNumerator, rDenominator)
  property("var / var             deriv w.r.t. left") = specGen.derivVarVarWrtLeft(rNumerator, rDenominator)
  property("var / var             deriv w.r.t. right") = specGen.derivVarVarWrtRight(rNumerator, rDenominator)
  property("var / var             deriv w.r.t. unknown") = specGen.derivVarVarWrtUnknown(rNumerator, rDenominator)
  property("var / var             deriv w.r.t. self") = specGen.derivVarVarWrtSelf(rDenominator)

  property("scalar / scalar       propagete value") = specGen.propagateScalarScalarWithNCValue(rNumerator, rDenominator, rExceptNaN)
  property("scalar / scalar       propagete container") = specGen.propagateScalarScalarWithCValue(rNumerator, rDenominator, rExceptNaN)
  property("scalar / container    propagete value") = specGen.propagateScalarContainerWithNCValue(rNumerator, rDenominator, rExceptNaN)
  property("scalar / container    propagete container") = specGen.propagateScalarContainerWithCValue(rNumerator, rDenominator, rExceptNaN)
  property("scalar / var          propagete value") = specGen.propagateScalarVarWithNCValue(rNumerator, rDenominator, rExceptNaN)
  property("scalar / var          propagete container") = specGen.propagateScalarVarWithCValue(rNumerator, rDenominator, rExceptNaN)
  property("container / scalar    propagete value") = specGen.propagateContainerScalarWithNCValue(rNumerator, rDenominator, rExceptNaN)
  property("container / scalar    propagete container") = specGen.propagateContainerScalarWithCValue(rNumerator, rDenominator, rExceptNaN)
  property("container / container propagete value") = specGen.propagateContainerContainerWithNCValue(rNumerator, rDenominator, rExceptNaN)
  property("container / container propagete container") = specGen.propagateContainerContainerWithCValue(rNumerator, rDenominator, rExceptNaN)
  property("container / var       propagete value") = specGen.propagateContainerVarWithNCValue(rNumerator, rDenominator, rExceptNaN)
  property("container / var       propagete container") = specGen.propagateContainerVarWithCValue(rNumerator, rDenominator, rExceptNaN)
  property("var / scalar          propagete value") = specGen.propagateVarScalarWithNCValue(rNumerator, rDenominator, rExceptNaN)
  property("var / scalar          propagete container") = specGen.propagateVarScalarWithCValue(rNumerator, rDenominator, rExceptNaN)
  property("var / container       propagete value") = specGen.propagateVarContainerWithNCValue(rNumerator, rDenominator, rExceptNaN)
  property("var / container       propagete container") = specGen.propagateVarContainerWithCValue(rNumerator, rDenominator, rExceptNaN)
  property("var / var             propagete value") = specGen.propagateVarVarWithNCValue(rNumerator, rDenominator, rExceptNaN)
  property("var / var             propagete container") = specGen.propagateVarVarWithCValue(rNumerator, rDenominator, rExceptNaN)

  property("scalar / scalar       grad") = specGen.gradScalarScalar(rNumerator, rDenominator)
  property("scalar / container    grad") = specGen.gradScalarContainer(rNumerator, rDenominator)
  property("scalar / var          grad") = specGen.gradScalarVar(rNumerator, rDenominator)
  property("container / scalar    grad") = specGen.gradContainerScalar(rNumerator, rDenominator)
  property("container / container grad") = specGen.gradContainerContainer(rNumerator, rDenominator)
  property("container / var       grad") = specGen.gradContainerVar(rNumerator, rDenominator)
  property("var / scalar          grad") = specGen.gradVarScalar(rNumerator, rDenominator)
  property("var / container       grad") = specGen.gradVarContainer(rNumerator, rDenominator)
  property("var / var             grad") = specGen.gradVarVar(rNumerator, rDenominator)

}


class DivSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends BinaryOpExpectedBehaviorDef[Seq, Float] {

  override val zero = 0f
  val one = 1f

  def one(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(1f)

  override def op(a: Node[Seq, Float], b: Node[Seq, Float]): Node[Seq, Float] = Div(a, b)

  override def applyScalarScalar(a: Float, b: Float): Float = a / b

  override def applyScalarContainer(a: Float, b: Seq[Float]): Seq[Float] = b.map(a / _)

  override def applyScalarVar(a: Float, b: Seq[Float]): Seq[Float] = b.map(a / _)

  override def applyContainerScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(_ / b)

  override def applyContainerContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x / y }

  override def applyContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x / y }

  override def applyVarScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(_ / b)

  override def applyVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x / y }

  override def applyVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x / y }

  override def derivScalarVarWrtRight(a: Float, b: Seq[Float]): Seq[Float] = b.map { y => -a / y / y }

  override def derivContainerVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => -x / y / y }

  override def derivVarContainerWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = b.map(1f / _)

  override def derivVarScalarWrtLeft(a: Seq[Float], b: Float): Seq[Float] = Seq.fill(a.size)(1f / b)

  override def derivVarVarWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = b.map(1f / _)

  override def derivVarVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => -x / y / y }

  override def derivVarVarWrtSelf(a: Seq[Float]): Seq[Float] = zero(a)

  override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

  override def propagateScalarVarWithNCValue(a: Float, b: Seq[Float], c: Float): Seq[Float] = b.map { y => -a / y / y * c }

  override def propagateScalarVarWithCValue(a: Float, b: Seq[Float], c: Seq[Float]): Seq[Float] = b.zip(c).map { case (y, z) => -a / y / y * z }

  override def propagateContainerVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.zip(b).map { case (x, y) => -x / y / y * c }

  override def propagateContainerVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => -x / y / y * z }

  override def propagateVarScalarWithCValue(a: Seq[Float], b: Float, c: Seq[Float]): Seq[Float] = a.zip(c).map { case (x, z) => z / b }

  override def propagateVarScalarWithNCValue(a: Seq[Float], b: Float, c: Float): Seq[Float] = Seq.fill(a.size)(c / b)

  override def propagateVarContainerWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = b.map(c / _)

  override def propagateVarContainerWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = b.zip(c).map { case (y, z) => z / y }

  override def propagateVarVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.zip(b).map { case (x, y) => c / y - x / y / y * c }

  override def propagateVarVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => z / y - x / y / y * z }


  override def gradScalarVar(a: Float, b: Seq[Float]): Seq[Float] = b.map { y => -a / y / y }

  override def gradContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => -x / y / y }

  override def gradVarScalar(a: Seq[Float], b: Float): Seq[Float] = Seq.fill(a.size)(1f / b)

  override def gradVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = b.map(1f / _)

  override def gradVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => 1f / y - x / y / y }

}
