package ch.grigala.scalismoad.test.graph.math.spec

import ch.grigala.scalismoad.graph.{Node, acos}
import ch.grigala.scalismoad.rule._
import ch.grigala.scalismoad.test.helper.gen.{SeqFloatNodeGenWithValueRange, SeqFloatValueGenWithValueRange}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatSoftCompareRule
import ch.grigala.scalismoad.test.helper.gen._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import ch.grigala.scalismoad.test.helper.specgen.UnaryOpExpectedBehaviorDef
import org.scalacheck.Properties


object AcosSpecSeqFloat extends Properties("Acos - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGenWithValueRange(-1f, 1f)
    val valueGen = new SeqFloatValueGenWithValueRange(-1f, 1f)
    val expects = new AcosSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("acos(a) apply") = seqFloatSpecGen.applyScalar()
    property("acos(a) apply") = seqFloatSpecGen.applyContainer()
    property("acos(a) apply") = seqFloatSpecGen.applyVar()

    property("acos(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("acos(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("acos(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("acos(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("acos(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("acos(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("acos(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("acos(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("acos(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("acos(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("acos(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("acos(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("acos(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class AcosSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = acos(node)

    override def applyScalar(a: Float): Float = scala.math.acos(a).toFloat

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.acos(x.toDouble).toFloat }

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.acos(x.toDouble).toFloat }

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x => -1f / scala.math.sqrt(1f - x * x).toFloat }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => -b / scala.math.sqrt(1f - x * x).toFloat }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) =>
        -y / scala.math.sqrt(1f - x * x).toFloat
    }

    override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => -1f / scala.math.sqrt(1f - x * x).toFloat }

}
