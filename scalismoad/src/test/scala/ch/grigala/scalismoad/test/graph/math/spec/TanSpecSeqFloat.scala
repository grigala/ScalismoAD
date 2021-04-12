package ch.grigala.scalismoad.test.graph.math.spec

import ch.grigala.scalismoad.graph.{Node, tan}
import ch.grigala.scalismoad.rule._
import ch.grigala.scalismoad.test.helper.gen.{SeqFloatNodeGenWithValueRange, SeqFloatValueGenWithValueRange}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatSoftCompareRule
import ch.grigala.scalismoad.test.helper.gen._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import ch.grigala.scalismoad.test.helper.specgen.UnaryOpExpectedBehaviorDef
import org.scalacheck.Properties


object TanSpecSeqFloat extends Properties("Tan - Seq[Float]") {

    implicit val compareRule = new SeqFloatSoftCompareRule

    val nodeGen = new SeqFloatNodeGenWithValueRange(-1e15f, 1e15f)
    val valueGen = new SeqFloatValueGenWithValueRange(-1e15f, 1e15f)
    val expects = new TanSeqFloatExpectedBehavior

    val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

    property("tan(a) apply") = seqFloatSpecGen.applyScalar()
    property("tan(a) apply") = seqFloatSpecGen.applyContainer()
    property("tan(a) apply") = seqFloatSpecGen.applyVar()

    property("tan(a) (scalar) w.r.t. unknown var") = seqFloatSpecGen.derivScalarWrtUnknownVar()
    property("tan(a) (container) w.r.t. unknown var") = seqFloatSpecGen.derivContainerWrtUnknownVar()
    property("tan(a) (var) w.r.t. a") = seqFloatSpecGen.derivVarWrtSelf()
    property("tan(a) (var) w.r.t. unknonw var") = seqFloatSpecGen.derivVarWrtUnknownVar()

    property("tan(a) (scalar) propagete with value") = seqFloatSpecGen.propagateScalarWithNCValue()
    property("tan(a) (scalar) propagate with container") = seqFloatSpecGen.propagateScalarWithCValue()
    property("tan(a) (container) propagete with value") = seqFloatSpecGen.propagateContainerWithNCValue()
    property("tan(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
    property("tan(a) (var) propagete with value") = seqFloatSpecGen.propagateVarWithNCValue()
    property("tan(a) (var) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

    property("tan(a) (scalar) grad") = seqFloatSpecGen.gradScalar()
    property("tan(a) (container) grad") = seqFloatSpecGen.gradContainer()
    property("tan(a) (var) grad") = seqFloatSpecGen.gradVar()

}


class TanSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

    override val zero: Float = 0f

    override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

    override def op(node: Node[Seq, Float]): Node[Seq, Float] = tan(node)

    override def applyScalar(a: Float): Float = scala.math.tan(a).toFloat

    override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.tan(x.toDouble).toFloat }

    override def applyVar(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.tan(x.toDouble).toFloat }

    override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = a.map { x =>
        val tanx = scala.math.tan(x.toDouble).toFloat
        1f + tanx * tanx
    }

    override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x =>
        val tanx = scala.math.tan(x.toDouble).toFloat
        b * (1f + tanx * tanx)
    }

    override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) =>
        val tanx = scala.math.tan(x.toDouble).toFloat
        y * (1f + tanx * tanx)
    }

    override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x =>
        val tanx = scala.math.tan(x.toDouble).toFloat
        1f + tanx * tanx
    }

}
