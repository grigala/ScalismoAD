package ch.grigala.ad.test.helper.rule

import ch.grigala.ad.graph.Scalar


class ScalarIntCompareRule extends CompareRule[Scalar, Int] {

    def shouldBe(a: Scalar[Int], b: Scalar[Int])(implicit d: DummyImplicit): Boolean = a.data == b.data

    def shouldBe(a: Int, b: Int): Boolean = a == b

}

