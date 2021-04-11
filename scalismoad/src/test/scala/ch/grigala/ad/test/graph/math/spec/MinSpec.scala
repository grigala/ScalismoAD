package ch.grigala.ad.test.graph.math.spec

import ch.grigala.ad.graph.{Node, min}
import ch.grigala.ad.rule._
import ch.grigala.ad.test.helper.gen.{SeqFloatNodeGen, SeqFloatValueGen}
import ch.grigala.ad.test.helper.rule.SeqFloatSoftCompareRule
import ch.grigala.ad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.ad.test.helper.specgen.{BinaryOpExpectedBehaviorDef, BinaryOpSpec}
import org.scalacheck.Properties

object MinSpecSeqFloat extends Properties("Min - Seq[Float]") {


    val nodeGen = new SeqFloatNodeGen
    val valueGen = new SeqFloatValueGen
    val expects = new MinSeqFloatExpectedBehavior
    implicit val compareRule = new SeqFloatSoftCompareRule

    val specGen = new BinaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("min( scalar, scalar )       apply") = specGen.applyScalarScalar()
    property("min( scalar, container )    apply") = specGen.applyScalarContainer()
    property("min( scalar, var )          apply") = specGen.applyScalarVar()
    property("min( container, scalar )    apply") = specGen.applyContainerScalar()
    property("min( container, container ) apply") = specGen.applyContainerContainer()
    property("min( container, var )       apply") = specGen.applyContainerVar()
    property("min( var, scalar )          apply") = specGen.applyVarScalar()
    property("min( var, container )       apply") = specGen.applyVarContainer()
    property("min( var, var )             apply") = specGen.applyVarVar()

    property("min( scalar, var )          deriv w.r.t. right") = specGen.derivScalarVarWrtRight()
    property("min( scalar, var )          deriv w.r.t. unknown") = specGen.derivScalarVarWrtUnknown()
    property("min( container, var )       deriv w.r.t. right") = specGen.derivContainerVarWrtRight()
    property("min( container, var )       deriv w.r.t. unknown") = specGen.derivContainerVarWrtUnknown()
    property("min( var, scalar )          deriv w.r.t. left") = specGen.derivVarScalarWrtLeft()
    property("min( var, scalar )          deriv w.r.t. unknown") = specGen.derivVarScalarWrtUnknown()
    property("min( var, container )       deriv w.r.t. left") = specGen.derivVarContainerWrtLeft()
    property("min( var, container )       deriv w.r.t. unknown") = specGen.derivVarContainerWrtUnknown()
    property("min( var, var )             deriv w.r.t. left") = specGen.derivVarVarWrtLeft()
    property("min( var, var )             deriv w.r.t. right") = specGen.derivVarVarWrtRight()
    property("min( var, var )             deriv w.r.t. unknown") = specGen.derivVarVarWrtUnknown()
    property("min( var, var )             deriv w.r.t. self") = specGen.derivVarVarWrtSelf()

    property("min( scalar, scalar )       propagete value") = specGen.propagateScalarScalarWithNCValue()
    property("min( scalar, scalar )       propagete container") = specGen.propagateScalarScalarWithCValue()
    property("min( scalar, container )    propagete value") = specGen.propagateScalarContainerWithNCValue()
    property("min( scalar, container )    propagete container") = specGen.propagateScalarContainerWithCValue()
    property("min( scalar, var )          propagete value") = specGen.propagateScalarVarWithNCValue()
    property("min( scalar, var )          propagete container") = specGen.propagateScalarVarWithCValue()
    property("min( container, scalar )    propagete value") = specGen.propagateContainerScalarWithNCValue()
    property("min( container, scalar )    propagete container") = specGen.propagateContainerScalarWithCValue()
    property("min( container, container ) propagete value") = specGen.propagateContainerContainerWithNCValue()
    property("min( container, container ) propagete container") = specGen.propagateContainerContainerWithCValue()
    property("min( container, var )       propagete value") = specGen.propagateContainerVarWithNCValue()
    property("min( container, var )       propagete container") = specGen.propagateContainerVarWithCValue()
    property("min( var, scalar )          propagete value") = specGen.propagateVarScalarWithNCValue()
    property("min( var, scalar )          propagete container") = specGen.propagateVarScalarWithCValue()
    property("min( var, container )       propagete value") = specGen.propagateVarContainerWithNCValue()
    property("min( var, container )       propagete container") = specGen.propagateVarContainerWithCValue()
    property("min( var, var )             propagete value") = specGen.propagateVarVarWithNCValue()
    property("min( var, var )             propagete container") = specGen.propagateVarVarWithCValue()

    property("min( scalar, scalar )       grad") = specGen.gradScalarScalar()
    property("min( scalar, container )    grad") = specGen.gradScalarContainer()
    property("min( scalar, var )          grad") = specGen.gradScalarVar()
    property("min( container, scalar )    grad") = specGen.gradContainerScalar()
    property("min( container, container ) grad") = specGen.gradContainerContainer()
    property("min( container, var )       grad") = specGen.gradContainerVar()
    property("min( var, scalar )          grad") = specGen.gradVarScalar()
    property("min( var, container )       grad") = specGen.gradVarContainer()
    property("min( var, var )             grad") = specGen.gradVarVar()

}


class MinSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends BinaryOpExpectedBehaviorDef[Seq, Float] {

    import scala.math.{min => fmin}

    override val zero = 0f

    val one = 1f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    def one(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(1f)

    override def op(a: Node[Seq, Float], b: Node[Seq, Float]): Node[Seq, Float] = min(a, b)

    override def applyScalarScalar(a: Float, b: Float): Float = fmin(a, b)

    override def applyScalarContainer(a: Float, b: Seq[Float]): Seq[Float] = b.map(fmin(a, _))

    override def applyScalarVar(a: Float, b: Seq[Float]): Seq[Float] = b.map(fmin(a, _))

    override def applyContainerScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(fmin(_, b))

    override def applyContainerContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => fmin(x, y) }

    override def applyContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => fmin(x, y) }

    override def applyVarScalar(a: Seq[Float], b: Float): Seq[Float] = a.map(fmin(_, b))

    override def applyVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => fmin(x, y) }

    override def applyVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => fmin(x, y) }


    override def derivScalarVarWrtRight(a: Float, b: Seq[Float]): Seq[Float] = b.map { y => if (a <= y) zero else one }

    override def derivContainerVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) zero else one }

    override def derivVarContainerWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) one else zero }

    override def derivVarScalarWrtLeft(a: Seq[Float], b: Float): Seq[Float] = a.map { x => if (x <= b) one else zero }

    override def derivVarVarWrtLeft(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) one else zero }

    override def derivVarVarWrtRight(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) zero else one }

    override def derivVarVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => one }


    override def propagateScalarVarWithNCValue(a: Float, b: Seq[Float], c: Float): Seq[Float] = b.map { y => if (a <= y) zero else c * one }

    override def propagateScalarVarWithCValue(a: Float, b: Seq[Float], c: Seq[Float]): Seq[Float] = b.zip(c).map { case (y, z) => if (a <= y) zero else z * one }

    override def propagateContainerVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) zero else c * one }

    override def propagateContainerVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => if (x <= y) zero else z * one }

    override def propagateVarScalarWithCValue(a: Seq[Float], b: Float, c: Seq[Float]): Seq[Float] = a.zip(c).map { case (x, z) => if (x <= b) z * one else zero }

    override def propagateVarScalarWithNCValue(a: Seq[Float], b: Float, c: Float): Seq[Float] = a.map { x => if (x <= b) c * one else zero }

    override def propagateVarContainerWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) c * one else zero }

    override def propagateVarContainerWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => if (x <= y) z * one else zero }

    override def propagateVarVarWithNCValue(a: Seq[Float], b: Seq[Float], c: Float): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) c * one else c * one }

    override def propagateVarVarWithCValue(a: Seq[Float], b: Seq[Float], c: Seq[Float]): Seq[Float] = a.zip(b).zip(c).map { case ((x, y), z) => if (x <= y) z * one else z * one }


    override def gradScalarVar(a: Float, b: Seq[Float]): Seq[Float] = b.map { y => if (a <= y) zero else one }

    override def gradContainerVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) zero else one }

    override def gradVarScalar(a: Seq[Float], b: Float): Seq[Float] = a.map { x => if (x <= b) one else zero }

    override def gradVarContainer(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) one else zero }

    override def gradVarVar(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) => if (x <= y) one else one }

}
