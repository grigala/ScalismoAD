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


object ScalarConstSpec extends Properties("ScalarConst") {

    implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

    implicit val scalarIntCompareRule = new ScalarIntCompareRule

    val genScalarIntType = new ScalarConstSpecGen[Scalar, Int](new ScalarIntNodeGen, new ScalarIntValueGen)

    val genSeqFloatType = new ScalarConstSpecGen[Seq, Float](new SeqFloatNodeGen, new SeqFloatValueGen)

    property("[Scalar, Int] - apply") = genScalarIntType.apply
    property("[Scalar, Int] - deriv w.r.t. unknown var") = genScalarIntType.derivUnknownVar
    property("[Scalar, Int] - propagate value") = genScalarIntType.propagate
    property("[Scalar, Int] - grad") = genScalarIntType.grad

    property("[Seq, Float]  - apply") = genSeqFloatType.apply
    property("[Seq, Float]  - deriv w.r.t. unknown var") = genSeqFloatType.derivUnknownVar
    property("[Seq, Float]  - propagate value") = genSeqFloatType.propagate
    property("[Seq, Float]  - grad") = genSeqFloatType.grad

}


class ScalarConstSpecGen[U[_], T](nodes: GenNode[U, T], values: GenValue[U, T])(implicit rule: ValueRule[U, T], compare: CompareRule[U, T]) {

    def apply: Prop = forAll(nodes.genScalarConst()) {
        c => c.apply() shouldBe c.data
    }

    def derivUnknownVar = forAll(nodes.genScalarConst(), nodes.genVar()) {
        (c, v) => c.deriv(v) shouldBe rule.zero(v())
    }

    def propagate = forAll(nodes.genScalarConst(), values.genValue()) {
        (c, v) => c.propagate(v) shouldBe rule.zero(v) * v
    }

    def grad = forAll(nodes.genScalarConst()) {
        c => c.grad() shouldBe rule.zero
    }

}
