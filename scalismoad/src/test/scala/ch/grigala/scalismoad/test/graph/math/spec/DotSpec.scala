package ch.grigala.scalismoad.test.graph.math.spec

import ch.grigala.scalismoad.graph.{Node, dot}
import ch.grigala.scalismoad.rule._
import ch.grigala.scalismoad.test.helper.gen.{SeqFloatNodeGenWithValueRange, SeqFloatValueGenWithValueRange}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatSoftCompareRule
import ch.grigala.scalismoad.test.helper.gen._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.specgen.{BinaryOpExpectedBehaviorDef, BinaryOpSpec}
import ch.grigala.scalismoad.test.helper.specgen.BinaryOpSpec
import org.scalacheck.Properties


object DotSpecSeqFloat extends Properties("Dot - Seq[Float]") {

    val nodeGen = new SeqFloatNodeGenWithValueRange
    val valueGen = new SeqFloatValueGenWithValueRange
    val expects = new DotSeqFloatExpectedBehavior
    implicit val compareRule = new SeqFloatSoftCompareRule

    val specGen = new BinaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    val r = (x: Float) => !x.equals(Float.NaN) && !x.equals(Float.PositiveInfinity) && !x.equals(Float.NegativeInfinity)

    property("scalar dot scalar       apply") = specGen.applyScalarScalar(r, r)
    property("scalar dot container    apply") = specGen.applyScalarContainer(r, r)
    property("scalar dot var          apply") = specGen.applyScalarVar(r, r)
    property("container dot scalar    apply") = specGen.applyContainerScalar(r, r)
    property("container dot container apply") = specGen.applyContainerContainer(r, r)
    property("container dot var       apply") = specGen.applyContainerVar(r, r)
    property("var dot scalar          apply") = specGen.applyVarScalar(r, r)
    property("var dot container       apply") = specGen.applyVarContainer(r, r)
    property("var dot var             apply") = specGen.applyVarVar(r, r)

    property("scalar dot var          deriv w.r.t. right") = specGen.derivScalarVarWrtRight(r, r)
    property("scalar dot var          deriv w.r.t. unknown") = specGen.derivScalarVarWrtUnknown(r, r)
    property("container dot var       deriv w.r.t. right") = specGen.derivContainerVarWrtRight(r, r)
    property("container dot var       deriv w.r.t. unknown") = specGen.derivContainerVarWrtUnknown(r, r)
    property("var dot scalar          deriv w.r.t. left") = specGen.derivVarScalarWrtLeft(r, r)
    property("var dot scalar          deriv w.r.t. unknown") = specGen.derivVarScalarWrtUnknown(r, r)
    property("var dot container       deriv w.r.t. left") = specGen.derivVarContainerWrtLeft(r, r)
    property("var dot container       deriv w.r.t. unknown") = specGen.derivVarContainerWrtUnknown(r, r)
    property("var dot var             deriv w.r.t. left") = specGen.derivVarVarWrtLeft(r, r)
    property("var dot var             deriv w.r.t. right") = specGen.derivVarVarWrtRight(r, r)
    property("var dot var             deriv w.r.t. unknown") = specGen.derivVarVarWrtUnknown(r, r)
    property("var dot var             deriv w.r.t. self") = specGen.derivVarVarWrtSelf(r)

    property("scalar dot scalar       propagete value") = specGen.propagateScalarScalarWithNCValue(r, r, r)
    property("scalar dot scalar       propagete container") = specGen.propagateScalarScalarWithCValue(r, r, r)
    property("scalar dot container    propagete value") = specGen.propagateScalarContainerWithNCValue(r, r, r)
    property("scalar dot container    propagete container") = specGen.propagateScalarContainerWithCValue(r, r, r)
    property("scalar dot var          propagete value") = specGen.propagateScalarVarWithNCValue(r, r, r)
    property("scalar dot var          propagete container") = specGen.propagateScalarVarWithCValue(r, r, r)
    property("container dot scalar    propagete value") = specGen.propagateContainerScalarWithNCValue(r, r, r)
    property("container dot scalar    propagete container") = specGen.propagateContainerScalarWithCValue(r, r, r)
    property("container dot container propagete value") = specGen.propagateContainerContainerWithNCValue(r, r, r)
    property("container dot container propagete container") = specGen.propagateContainerContainerWithCValue(r, r, r)
    property("container dot var       propagete value") = specGen.propagateContainerVarWithNCValue(r, r, r)
    property("container dot var       propagete container") = specGen.propagateContainerVarWithCValue(r, r, r)
    property("var dot scalar          propagete value") = specGen.propagateVarScalarWithNCValue(r, r, r)
    property("var dot scalar          propagete container") = specGen.propagateVarScalarWithCValue(r, r, r)
    property("var dot container       propagete value") = specGen.propagateVarContainerWithNCValue(r, r, r)
    property("var dot container       propagete container") = specGen.propagateVarContainerWithCValue(r, r, r)
    property("var dot var             propagete value") = specGen.propagateVarVarWithNCValue(r, r, r)
    property("var dot var             propagete container") = specGen.propagateVarVarWithCValue(r, r, r)

    property("scalar dot scalar       grad") = specGen.gradScalarScalar(r, r)
    property("scalar dot container    grad") = specGen.gradScalarContainer(r, r)
    property("scalar dot var          grad") = specGen.gradScalarVar(r, r)
    property("container dot scalar    grad") = specGen.gradContainerScalar(r, r)
    property("container dot container grad") = specGen.gradContainerContainer(r, r)
    property("container dot var       grad") = specGen.gradContainerVar(r, r)
    property("var dot scalar          grad") = specGen.gradVarScalar(r, r)
    property("var dot container       grad") = specGen.gradVarContainer(r, r)
    property("var dot var             grad") = specGen.gradVarVar(r, r)

}


class DotSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends BinaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero = 0f
    val one = 1f

    def one(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(1f)

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(a: Node[Seq, Float], b: Node[Seq, Float]): Node[Seq, Float] = dot(a, b)

    override def applyScalarScalar(a: Float, b: Float): Float = a * b

    override def applyScalarContainer(a: Float, b: Seq[Float]): Seq[Float] = Seq(sum(b.map(a * _)))

    override def applyScalarVar(a: Float, b: Seq[Float]): Seq[Float] = Seq(sum(b.map(a * _)))

    override def applyContainerScalar(a: Seq[Float], b: Float): Seq[Float] = Seq(sum(a.map(_ * b)))

    override def applyContainerContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a.zip(b).map({ case (x, y) => x * y })))

    override def applyContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a.zip(b).map({ case (x, y) => x * y })))

    override def applyVarScalar(a: Seq[Float], b: Float): Seq[Float] = Seq(sum(a.map(_ * b)))

    override def applyVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a.zip(b).map({ case (x, y) => x * y })))

    override def applyVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a.zip(b).map({ case (x, y) => x * y })))

    override def derivScalarVarWrtRight(a: Float, b: Seq[Float]): Seq[Float] = Seq(sum(b.map({ _ => a })))

    override def derivContainerVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a.zip(b).map({ case (x, _) => x })))

    override def derivVarContainerWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a.zip(b).map({ case (_, y) => y })))

    override def derivVarScalarWrtLeft(a: Seq[Float], b: Float): Seq[Float] = Seq(sum(a.map({ _ => b })))

    override def derivVarVarWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a.zip(b).map({ case (_, y) => y })))

    override def derivVarVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a.zip(b).map({ case (x, _) => x })))

    override def derivVarVarWrtSelf(a: Seq[Float]): Seq[Float] = Seq(sum(a.map({ case x => 2f * x })))

    def sum(s: Seq[Float]): Float = s.foldLeft(0f)(_ + _)

    override def propagateScalarVarWithNCValue(a: Float, b: Seq[Float], c: Float): Seq[Float] = Seq(a * c)

    override def propagateScalarVarWithCValue(a: Float, b: Seq[Float], c: Seq[Float]): Seq[Float] = Seq(sum(c) * a)

    override def propagateContainerVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = Seq(sum(a.map({ x => x * c })))

    override def propagateContainerVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = Seq(sum(a.zip(c).map({ case (x, z) => x * z })))

    override def propagateVarScalarWithCValue(a: Seq[Float], b: Float, c: Seq[Float]): Seq[Float] = Seq(sum(c.map({ z => b * z })))

    override def propagateVarScalarWithNCValue(a: Seq[Float], b: Float, c: Float): Seq[Float] = Seq(b * c)

    override def propagateVarContainerWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = Seq(sum(b.map({ y => y * c })))

    override def propagateVarContainerWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = Seq(sum(b.zip(c).map({ case (y, z) => y * z })))

    override def propagateVarVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = Seq(sum(a) * c + sum(b) * c)

    override def propagateVarVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = {
        Seq(sum(a.zip(c).map { case (x, z) => x * z }) + sum(b.zip(c).map { case (y, z) => y * z }))
    }


    override def gradScalarVar(a: Float, b: Seq[Float]): Seq[Float] = Seq.fill(b.size)(a)

    override def gradContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a))

    override def gradVarScalar(a: Seq[Float], b: Float): Seq[Float] = Seq(b)

    override def gradVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(b))

    override def gradVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = Seq(sum(a) + sum(b))

}
