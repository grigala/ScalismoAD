package com.kogecoo.scalaad.test.graph.math.spec

import ch.grigala.ad.graph.{Node, max}
import ch.grigala.ad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{BinaryOpExpectedBehaviorDef, BinaryOpSpec}
import org.scalacheck.Properties


object MaxSpecSeqFloat extends Properties("Max - Seq[Float]") {


    val nodeGen = new SeqFloatNodeGen
    val valueGen = new SeqFloatValueGen
    val expects = new MaxSeqFloatExpectedBehavior
    implicit val compareRule = new SeqFloatSoftCompareRule

    val specGen = new BinaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("max( scalar, scalar )       apply") = specGen.applyScalarScalar()
    property("max( scalar, container )    apply") = specGen.applyScalarContainer()
    property("max( scalar, var )          apply") = specGen.applyScalarVar()
    property("max( container, scalar )    apply") = specGen.applyContainerScalar()
    property("max( container, container ) apply") = specGen.applyContainerContainer()
    property("max( container, var )       apply") = specGen.applyContainerVar()
    property("max( var, scalar )          apply") = specGen.applyVarScalar()
    property("max( var, container )       apply") = specGen.applyVarContainer()
    property("max( var, var )             apply") = specGen.applyVarVar()

    property("max( scalar, var )          deriv w.r.t. right") = specGen.derivScalarVarWrtRight()
    property("max( scalar, var )          deriv w.r.t. unknown") = specGen.derivScalarVarWrtUnknown()
    property("max( container, var )       deriv w.r.t. right") = specGen.derivContainerVarWrtRight()
    property("max( container, var )       deriv w.r.t. unknown") = specGen.derivContainerVarWrtUnknown()
    property("max( var, scalar )          deriv w.r.t. left") = specGen.derivVarScalarWrtLeft()
    property("max( var, scalar )          deriv w.r.t. unknown") = specGen.derivVarScalarWrtUnknown()
    property("max( var, container )       deriv w.r.t. left") = specGen.derivVarContainerWrtLeft()
    property("max( var, container )       deriv w.r.t. unknown") = specGen.derivVarContainerWrtUnknown()
    property("max( var, var )             deriv w.r.t. left") = specGen.derivVarVarWrtLeft()
    property("max( var, var )             deriv w.r.t. right") = specGen.derivVarVarWrtRight()
    property("max( var, var )             deriv w.r.t. unknown") = specGen.derivVarVarWrtUnknown()
    property("max( var, var )             deriv w.r.t. self") = specGen.derivVarVarWrtSelf()

    property("max( scalar, scalar )       propagete value") = specGen.propagateScalarScalarWithNCValue()
    property("max( scalar, scalar )       propagete container") = specGen.propagateScalarScalarWithCValue()
    property("max( scalar, container )    propagete value") = specGen.propagateScalarContainerWithNCValue()
    property("max( scalar, container )    propagete container") = specGen.propagateScalarContainerWithCValue()
    property("max( scalar, var )          propagete value") = specGen.propagateScalarVarWithNCValue()
    property("max( scalar, var )          propagete container") = specGen.propagateScalarVarWithCValue()
    property("max( container, scalar )    propagete value") = specGen.propagateContainerScalarWithNCValue()
    property("max( container, scalar )    propagete container") = specGen.propagateContainerScalarWithCValue()
    property("max( container, container ) propagete value") = specGen.propagateContainerContainerWithNCValue()
    property("max( container, container ) propagete container") = specGen.propagateContainerContainerWithCValue()
    property("max( container, var )       propagete value") = specGen.propagateContainerVarWithNCValue()
    property("max( container, var )       propagete container") = specGen.propagateContainerVarWithCValue()
    property("max( var, scalar )          propagete value") = specGen.propagateVarScalarWithNCValue()
    property("max( var, scalar )          propagete container") = specGen.propagateVarScalarWithCValue()
    property("max( var, container )       propagete value") = specGen.propagateVarContainerWithNCValue()
    property("max( var, container )       propagete container") = specGen.propagateVarContainerWithCValue()
    property("max( var, var )             propagete value") = specGen.propagateVarVarWithNCValue()
    property("max( var, var )             propagete container") = specGen.propagateVarVarWithCValue()

    property("max( scalar, scalar )       grad") = specGen.gradScalarScalar()
    property("max( scalar, container )    grad") = specGen.gradScalarContainer()
    property("max( scalar, var )          grad") = specGen.gradScalarVar()
    property("max( container, scalar )    grad") = specGen.gradContainerScalar()
    property("max( container, container ) grad") = specGen.gradContainerContainer()
    property("max( container, var )       grad") = specGen.gradContainerVar()
    property("max( var, scalar )          grad") = specGen.gradVarScalar()
    property("max( var, container )       grad") = specGen.gradVarContainer()
    property("max( var, var )             grad") = specGen.gradVarVar()

}


class MaxSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends BinaryOpExpectedBehaviorDef[Seq, Float] {

    import scala.math.{max => fmax}

    override val zero = 0f

    val one = 1f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    def one(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(1f)

    override def op(a: Node[Seq, Float], b: Node[Seq, Float]): Node[Seq, Float] = max(a, b)

    override def applyScalarScalar(a: Float, b: Float): Float = fmax(a, b)

    override def applyScalarContainer(a: Float, b: Seq[Float]): Seq[Float] = b.map(fmax(a, _))

    override def applyScalarVar(a: Float, b: Seq[Float]): Seq[Float] = b.map(fmax(a, _))

    override def applyContainerScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(fmax(_, b))

    override def applyContainerContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => fmax(x, y) }

    override def applyContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => fmax(x, y) }

    override def applyVarScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(fmax(_, b))

    override def applyVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => fmax(x, y) }

    override def applyVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => fmax(x, y) }


    override def derivScalarVarWrtRight(a: Float, b: Seq[Float]): Seq[Float] = b.map { y => if (a >= y) zero else one }

    override def derivContainerVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) zero else one }

    override def derivVarContainerWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) one else zero }

    override def derivVarScalarWrtLeft(a: Seq[Float], b: Float): Seq[Float] = a.map { x => if (x >= b) one else zero }

    override def derivVarVarWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) one else zero }

    override def derivVarVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) zero else one }

    override def derivVarVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => one }


    override def propagateScalarVarWithNCValue(a: Float, b: Seq[Float], c: Float): Seq[Float] = b.map { y => if (a >= y) zero else c * one }

    override def propagateScalarVarWithCValue(a: Float, b: Seq[Float], c: Seq[Float]): Seq[Float] = b.zip(c).map { case (y, z) => if (a >= y) zero else z * one }

    override def propagateContainerVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) zero else c * one }

    override def propagateContainerVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => if (x >= y) zero else z * one }

    override def propagateVarScalarWithCValue(a: Seq[Float], b: Float, c: Seq[Float]): Seq[Float] = a.zip(c).map { case (x, z) => if (x >= b) z * one else zero }

    override def propagateVarScalarWithNCValue(a: Seq[Float], b: Float, c: Float): Seq[Float] = a.map { x => if (x >= b) c * one else zero }

    override def propagateVarContainerWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) c * one else zero }

    override def propagateVarContainerWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => if (x >= y) z * one else zero }

    override def propagateVarVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) c * one else c * one }

    override def propagateVarVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => if (x >= y) z * one else z * one }


    override def gradScalarVar(a: Float, b: Seq[Float]): Seq[Float] = b.map { y => if (a >= y) zero else one }

    override def gradContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) zero else one }

    override def gradVarScalar(a: Seq[Float], b: Float): Seq[Float] = a.map { x => if (x >= b) one else zero }

    override def gradVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) one else zero }

    override def gradVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x >= y) one else one }

}
