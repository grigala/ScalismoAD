package ch.grigala.ad.test.graph

import ch.grigala.ad.graph.Scalar
import ch.grigala.ad.rule.ValueRule
import ch.grigala.ad.test.helper.gen._
import ch.grigala.ad.test.helper.matcher.ValueMatcherProp._
import ch.grigala.ad.test.helper.rule.ScalarIntValueRule.Implicits._
import ch.grigala.ad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.ad.test.helper.rule.{CompareRule, ScalarIntCompareRule, SeqFloatExactCompareRule}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Prop, Properties}


object ContainerConstSpec extends Properties("ContainerConst") {

    implicit val scalarIntCompareRule = new ScalarIntCompareRule

    implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

    val genScalarIntType = new ContainerConstSpecGen[Scalar, Int](new ScalarIntNodeGen, new ScalarIntValueGen)

    val genSeqFloatType = new ContainerConstSpecGen[Seq, Float](new SeqFloatNodeGen, new SeqFloatValueGen)

    property("[Scalar, Int] - apply") = genScalarIntType.apply
    property("[Scalar, Int] - deriv w.r.t. unknown var") = genScalarIntType.derivUnknownVar
    property("[Scalar, Int] - propagate value") = genScalarIntType.propagate
    property("[Scalar, Int] - grad") = genScalarIntType.grad

    property("[Seq, Float]  - apply") = genSeqFloatType.apply
    property("[Seq, Float]  - deriv w.r.t. unknown var") = genSeqFloatType.derivUnknownVar
    property("[Seq, Float]  - propagate value") = genSeqFloatType.propagate
    property("[Seq, Float]  - grad") = genSeqFloatType.grad

}


class ContainerConstSpecGen[U[_], T](nodes: GenNode[U, T], values: GenValue[U, T])(implicit rule: ValueRule[U, T], compare: CompareRule[U, T]) {

    def apply: Prop = forAll(nodes.genContainerConst()) {
        c => c.apply() shouldBe c.data
    }

    def derivUnknownVar = forAll(nodes.genContainerConst(), nodes.genVar()) {
        (c, v) => c.deriv(v) shouldBe rule.zero(c())
    }

    def propagate = forAll(nodes.genContainerConst(), values.genValue()) {
        (c, v) => c.propagate(v) shouldBe rule.zero(c()) * v
    }

    def grad = forAll(nodes.genContainerConst()) {
        c => c.grad() shouldBe rule.zero(c())
    }

}
