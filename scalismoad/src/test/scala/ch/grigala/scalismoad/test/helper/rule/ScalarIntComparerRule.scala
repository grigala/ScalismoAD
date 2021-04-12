package ch.grigala.scalismoad.test.helper.rule

import ch.grigala.scalismoad.graph.Scalar


class ScalarIntCompareRule extends CompareRule[Scalar, Int] {

    def shouldBe(a: Scalar[Int], b: Scalar[Int])(implicit d: DummyImplicit): Boolean = a.data == b.data

    def shouldBe(a: Int, b: Int): Boolean = a == b

}

