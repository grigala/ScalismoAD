package ch.grigala.scalismoad.test.graph

import ch.grigala.scalismoad.graph.Scalar
import ch.grigala.scalismoad.rule.ValueRule
import ch.grigala.scalismoad.test.helper.gen.{GenNode, GenValue, ScalarIntNodeGen, ScalarIntValueGen, SeqFloatNodeGen, SeqFloatValueGen}
import ch.grigala.scalismoad.test.helper.rule.{CompareRule, ScalarIntCompareRule, SeqFloatExactCompareRule}
import ch.grigala.scalismoad.test.helper.gen._
import ch.grigala.scalismoad.test.helper.matcher.ValueMatcherProp._
import ch.grigala.scalismoad.test.helper.rule.ScalarIntValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.rule.ScalarIntCompareRule
import org.scalacheck.Prop.forAll
import org.scalacheck.{Prop, Properties}


object VarSpec extends Properties("Var") {

    implicit val scalarIntCompareRule = new ScalarIntCompareRule

    implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

    val genScalarIntType = new VarSpecGen[Scalar, Int](new ScalarIntNodeGen, new ScalarIntValueGen)

    val genSeqFloatType = new VarSpecGen[Seq, Float](new SeqFloatNodeGen, new SeqFloatValueGen)

    property("[Scalar, Int] - apply") = genScalarIntType.apply
    property("[Scalar, Int] - deriv w.r.t. self") = genScalarIntType.derivSelf
    property("[Scalar, Int] - deriv w.r.t. unknown Var") = genScalarIntType.derivUnknownVar
    property("[Scalar, Int] - propagate value") = genScalarIntType.propagate
    property("[Scalar, Int] - grad") = genScalarIntType.grad

    property("[Seq, Float]  - apply") = genSeqFloatType.apply
    property("[Seq, Float]  - deriv w.r.t. self") = genSeqFloatType.derivSelf
    property("[Seq, Float]  - deriv w.r.t. unknown Var") = genSeqFloatType.derivUnknownVar
    property("[Seq, Float]  - propagate value") = genSeqFloatType.propagate
    property("[Seq, Float]  - grad") = genSeqFloatType.grad

}


class VarSpecGen[U[_], T](nodes: GenNode[U, T], values: GenValue[U, T])(implicit rule: ValueRule[U, T], compare: CompareRule[U, T]) {

    def apply: Prop = forAll(nodes.genVar()) {
        c => c.apply() shouldBe c.data
    }

    def derivSelf = forAll(nodes.genVar()) {
        c => c.deriv(c) shouldBe rule.one(c())
    }

    def derivUnknownVar = forAll(nodes.genVar(), nodes.genVar()) {
        (c, v) => c.deriv(v) shouldBe rule.zero(v())
    }

    def propagate = forAll(nodes.genVar(), values.genValue()) {
        (c, v) => c.propagate(v) shouldBe rule.one(c()) * v
    }

    def grad = forAll(nodes.genVar()) {
        c => c.grad() shouldBe rule.one(c())
    }

}

