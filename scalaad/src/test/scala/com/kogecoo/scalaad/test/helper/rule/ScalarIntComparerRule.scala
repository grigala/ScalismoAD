package com.kogecoo.scalaad.test.helper.rule

import com.kogecoo.scalaad.graph.Scalar


class ScalarIntCompareRule extends CompareRule[Scalar, Int] {

    def shouldBe(a: Scalar[Int], b: Scalar[Int])(implicit d: DummyImplicit): Boolean = a.data == b.data

    def shouldBe(a: Int, b: Int): Boolean = a == b

}

