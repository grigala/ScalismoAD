package ch.grigala.scalismoad.test.graph.op.spec

import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.rule._
import ch.grigala.scalismoad.test.helper.gen.{SeqFloatNodeGen, SeqFloatValueGen}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatSoftCompareRule
import ch.grigala.scalismoad.test.helper.gen._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import ch.grigala.scalismoad.test.helper.specgen.UnaryOpExpectedBehaviorDef
import org.scalacheck.Properties


object TransposeSpecSeqFloat extends Properties("Transpose - Seq[Float]") {


    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGen
    val valueGen = new SeqFloatValueGen
    val expects = new TransposeSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("a.T apply") = seqFloatSpecGen.applyScalar()
    property("a.T apply") = seqFloatSpecGen.applyContainer()
    property("a.T apply") = seqFloatSpecGen.applyVar()

    property("a.T (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("a.T (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("a.T (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("a.T (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("a.T (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("a.T (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("a.T (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("a.T (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("a.T (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("a.T (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("a.T (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("a.T (container) grad") = seqFloatSpecGen.gradContainer()
    property("a.T (var) grad") = seqFloatSpecGen.gradVar()

}


class TransposeSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = Transpose(node)

    override def applyScalar(a: Float): Float = a

    override def applyContainer(a: Seq[Float]): Seq[Float] = a

    override def applyVar(a: Seq[Float]): Seq[Float] = a

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = Seq.fill(a.size)(1f)

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = Seq.fill(a.size)(1f * b)

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (_, y) => y }

    override def gradVar(a: Seq[Float]): Seq[Float] = Seq.fill(a.size)(1f)

}


