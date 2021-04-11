package ch.grigala.ad.test.graph.math.spec

import ch.grigala.ad.graph.{Node, abs}
import ch.grigala.ad.rule._
import ch.grigala.ad.test.helper.gen.{SeqFloatNodeGenWithValueRange, SeqFloatValueGenWithValueRange}
import ch.grigala.ad.test.helper.rule.SeqFloatSoftCompareRule
import ch.grigala.ad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.ad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import org.scalacheck.Properties


object AbsSpecSeqFloat extends Properties("Abs - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGenWithValueRange(-1e15f, 1e15f)
    val valueGen = new SeqFloatValueGenWithValueRange(-1e15f, 1e15f)
    val expects = new AbsSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("abs(a) apply") = seqFloatSpecGen.applyScalar()
    property("abs(a) apply") = seqFloatSpecGen.applyContainer()
    property("abs(a) apply") = seqFloatSpecGen.applyVar()

    property("abs(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("abs(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("abs(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("abs(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("abs(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("abs(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("abs(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("abs(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("abs(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("abs(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("abs(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("abs(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("abs(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class AbsSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = abs(node)

    override def applyScalar(a: Float): Float = scala.math.abs(a)

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map(scala.math.abs)

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map(scala.math.abs)

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = {
        a.map { x => if (x < 0) -1f else 1f }
    }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = {
        a.map { x => if (x < 0) -b else b }
    }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = {
        a.zip(b).map { case (x, y) => if (x < 0) -y else y }
    }

    override def gradVar(a: Seq[Float]): Seq[Float] = {
        a.map { x => if (x < 0) -1f else 1f }
    }

}
