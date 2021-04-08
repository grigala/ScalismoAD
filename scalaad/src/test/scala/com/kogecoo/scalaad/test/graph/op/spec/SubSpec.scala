package com.kogecoo.scalaad.test.graph.op.spec

import com.kogecoo.scalaad.graph.{Node, Sub}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{BinaryOpExpectedBehaviorDef, BinaryOpSpec}
import org.scalacheck.Properties




object SubSpecSeqFloat extends Properties("Sub - Seq[Float]") {

    val nodeGen = new SeqFloatNodeGen
    val valueGen = new SeqFloatValueGen
    val expects = new SubSeqFloatExpectedBehavior
    implicit val compareRule = new SeqFloatSoftCompareRule

    val specGen = new BinaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    val rExceptNaN = (x: Float) => !x.equals(Float.NaN)


    property("scalar - scalar       apply") = specGen.applyScalarScalar(rExceptNaN, rExceptNaN)
    property("scalar - container    apply") = specGen.applyScalarContainer(rExceptNaN, rExceptNaN)
    property("scalar - var          apply") = specGen.applyScalarVar(rExceptNaN, rExceptNaN)
    property("container - scalar    apply") = specGen.applyContainerScalar(rExceptNaN, rExceptNaN)
    property("container - container apply") = specGen.applyContainerContainer(rExceptNaN, rExceptNaN)
    property("container - var       apply") = specGen.applyContainerVar(rExceptNaN, rExceptNaN)
    property("var - scalar          apply") = specGen.applyVarScalar(rExceptNaN, rExceptNaN)
    property("var - container       apply") = specGen.applyVarContainer(rExceptNaN, rExceptNaN)
    property("var - var             apply") = specGen.applyVarVar(rExceptNaN, rExceptNaN)

    property("scalar - var          deriv w.r.t. right") = specGen.derivScalarVarWrtRight(rExceptNaN, rExceptNaN)
    property("scalar - var          deriv w.r.t. unknown") = specGen.derivScalarVarWrtUnknown(rExceptNaN, rExceptNaN)
    property("container - var       deriv w.r.t. right") = specGen.derivContainerVarWrtRight(rExceptNaN, rExceptNaN)
    property("container - var       deriv w.r.t. unknown") = specGen.derivContainerVarWrtUnknown(rExceptNaN, rExceptNaN)
    property("var - scalar          deriv w.r.t. left") = specGen.derivVarScalarWrtLeft(rExceptNaN, rExceptNaN)
    property("var - scalar          deriv w.r.t. unknown") = specGen.derivVarScalarWrtUnknown(rExceptNaN, rExceptNaN)
    property("var - container       deriv w.r.t. left") = specGen.derivVarContainerWrtLeft(rExceptNaN, rExceptNaN)
    property("var - container       deriv w.r.t. unknown") = specGen.derivVarContainerWrtUnknown(rExceptNaN, rExceptNaN)
    property("var - var             deriv w.r.t. left") = specGen.derivVarVarWrtLeft(rExceptNaN, rExceptNaN)
    property("var - var             deriv w.r.t. right") = specGen.derivVarVarWrtRight(rExceptNaN, rExceptNaN)
    property("var - var             deriv w.r.t. unknown") = specGen.derivVarVarWrtUnknown(rExceptNaN, rExceptNaN)
    property("var - var             deriv w.r.t. self") = specGen.derivVarVarWrtSelf(rExceptNaN)

    property("scalar - scalar       propagete value") = specGen.propagateScalarScalarWithNCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("scalar - scalar       propagete container") = specGen.propagateScalarScalarWithCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("scalar - container    propagete value") = specGen.propagateScalarContainerWithNCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("scalar - container    propagete container") = specGen.propagateScalarContainerWithCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("scalar - var          propagete value") = specGen.propagateScalarVarWithNCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("scalar - var          propagete container") = specGen.propagateScalarVarWithCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("container - scalar    propagete value") = specGen.propagateContainerScalarWithNCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("container - scalar    propagete container") = specGen.propagateContainerScalarWithCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("container - container propagete value") = specGen.propagateContainerContainerWithNCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("container - container propagete container") = specGen.propagateContainerContainerWithCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("container - var       propagete value") = specGen.propagateContainerVarWithNCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("container - var       propagete container") = specGen.propagateContainerVarWithCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("var - scalar          propagete value") = specGen.propagateVarScalarWithNCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("var - scalar          propagete container") = specGen.propagateVarScalarWithCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("var - container       propagete value") = specGen.propagateVarContainerWithNCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("var - container       propagete container") = specGen.propagateVarContainerWithCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("var - var             propagete value") = specGen.propagateVarVarWithNCValue(rExceptNaN, rExceptNaN, rExceptNaN)
    property("var - var             propagete container") = specGen.propagateVarVarWithCValue(rExceptNaN, rExceptNaN, rExceptNaN)

    property("scalar - scalar       grad") = specGen.gradScalarScalar(rExceptNaN, rExceptNaN)
    property("scalar - container    grad") = specGen.gradScalarContainer(rExceptNaN, rExceptNaN)
    property("scalar - var          grad") = specGen.gradScalarVar(rExceptNaN, rExceptNaN)
    property("container - scalar    grad") = specGen.gradContainerScalar(rExceptNaN, rExceptNaN)
    property("container - container grad") = specGen.gradContainerContainer(rExceptNaN, rExceptNaN)
    property("container - var       grad") = specGen.gradContainerVar(rExceptNaN, rExceptNaN)
    property("var - scalar          grad") = specGen.gradVarScalar(rExceptNaN, rExceptNaN)
    property("var - container       grad") = specGen.gradVarContainer(rExceptNaN, rExceptNaN)
    property("var - var             grad") = specGen.gradVarVar(rExceptNaN, rExceptNaN)

}


class SubSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends BinaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero = 0f
    val one = 1f

    override def op(a: Node[Seq, Float], b: Node[Seq, Float]): Node[Seq, Float] = Sub(a, b)

    override def applyScalarScalar(a: Float, b: Float): Float = a - b

    override def applyScalarContainer(a: Float, b: Seq[Float]): Seq[Float] = b.map(a - _)

    override def applyScalarVar(a: Float, b: Seq[Float]): Seq[Float] = b.map(a - _)

    override def applyContainerScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(_ - b)

    override def applyContainerContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x - y }

    override def applyContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x - y }

    override def applyVarScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(_ - b)

    override def applyVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x - y }

    override def applyVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => x - y }

    override def derivScalarVarWrtRight(a: Float, b: Seq[Float]): Seq[Float] = one(b).map(-_)

    override def derivContainerVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = one(b).map(-_)

    override def derivVarContainerWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = one(a)

    override def derivVarScalarWrtLeft(a: Seq[Float], b: Float): Seq[Float] = one(a)

    override def derivVarVarWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = one(a)

    override def derivVarVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = one(b).map(-_)

    override def derivVarVarWrtSelf(a: Seq[Float]): Seq[Float] = zero(a)

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def propagateScalarVarWithNCValue(a: Float, b: Seq[Float], c: Float): Seq[Float] = Seq.fill(b.size)(-c)

    override def propagateScalarVarWithCValue(a: Float, b: Seq[Float], c: Seq[Float]): Seq[Float] = c.map(-_)

    override def propagateContainerVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = Seq.fill(b.size)(-c)

    override def propagateContainerVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = c.map(-_)

    override def propagateVarScalarWithCValue(a: Seq[Float], b: Float, c: Seq[Float]): Seq[Float] = c

    override def propagateVarScalarWithNCValue(a: Seq[Float], b: Float, c: Float): Seq[Float] = Seq.fill(a.size)(c)

    override def propagateVarContainerWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = Seq.fill(a.size)(c)

    override def propagateVarContainerWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = c

    override def propagateVarVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = Seq.fill(a.size)(0f)

    override def propagateVarVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = Seq.fill(a.size)(0f)

    override def gradScalarVar(a: Float, b: Seq[Float]): Seq[Float] = one(b).map(-_)

    override def gradContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = one(b).map(-_)

    def one(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(1f)

    override def gradVarScalar(a: Seq[Float], b: Float): Seq[Float] = one(a)

    override def gradVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = one(a)

    override def gradVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq.fill(a.size)(0f)

}
