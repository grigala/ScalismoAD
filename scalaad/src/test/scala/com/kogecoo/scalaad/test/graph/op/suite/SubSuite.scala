package com.kogecoo.scalaad.test.graph.op.suite

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherAssert._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatExactCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue}
import org.scalatest.funsuite.AnyFunSuite


class SubSuite extends AnyFunSuite {

    implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

    test("Sub - Seq[Float]") {

        val var42_12 = Var[Seq, Float](Seq(42.0f, 12f))
        val var71_3 = Var(Seq(71.0f, 3f))
        val var33_51 = Var(Seq(33.0f, 51.0f))
        val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
        val c63_21 = ContainerConst[Seq, Float](Seq(63.0f, 21.0f))
        val sc2 = ScalarConst(2.0f)

        val value15 = NonContainerValue[Seq, Float](15.0f)
        val cValue8 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

        // ContainerConst - ContainerConst
        val a1 = Sub(c12_42, c63_21)

        a1.apply() shouldBe Seq(12f - 63f, 42f - 21f)
        a1.deriv(var42_12) shouldBe Seq(0f, 0f)
        a1.propagate(value15) shouldBe Seq(0f, 0f)
        a1.propagate(cValue8) shouldBe Seq(0.0f, 0.0f)

        // Var - ContainerConst
        val a2 = Sub(var71_3, c63_21)

        a2.apply() shouldBe Seq(71f - 63f, 3f - 21f)
        a2.deriv(var71_3) shouldBe Seq(1f, 1f)
        a2.deriv(var42_12) shouldBe Seq(0f, 0f)
        a2.propagate(value15) shouldBe Seq(15f, 15f)
        a2.propagate(cValue8) shouldBe Seq(3.0f, 5.0f)

        // ContainerConst - Var
        val a3 = Sub(c12_42, var33_51)

        a3.apply() shouldBe Seq(12f - 33f, 42f - 51f)
        a2.deriv(var42_12) shouldBe Seq(0f, 0f)
        a3.deriv(var33_51) shouldBe Seq(-1f, -1f)
        a3.propagate(value15) shouldBe Seq(-15f, -15f)
        a3.propagate(cValue8) shouldBe Seq(-3.0f, -5.0f)

        // Var - Var
        val a4 = Sub(var71_3, var33_51)

        a4.apply() shouldBe Seq(71f - 33f, 3f - 51f)
        a2.deriv(var42_12) shouldBe Seq(0f, 0f)
        a4.deriv(var71_3) shouldBe Seq(1f, 1f)
        a4.deriv(var33_51) shouldBe Seq(-1f, -1f)
        a4.propagate(value15) shouldBe Seq(0f, 0f)
        a4.propagate(cValue8) shouldBe Seq(0f, 0f)

        // more complex one
        // ((x - 2) - y), x = [71, 3], y = [33, 51]
        val a5 = Sub(Sub(var71_3, sc2), var33_51)
        a5.apply() shouldBe Seq(71f - 2f - 33f, 3f - 2f - 51f)
        a2.deriv(var42_12) shouldBe Seq(0f, 0f)
        a5.deriv(var71_3) shouldBe Seq(1f, 1f)
        a5.deriv(var33_51) shouldBe Seq(-1f, -1f)
        a5.propagate(value15) shouldBe Seq(0f, 0f)
        a5.propagate(cValue8) shouldBe Seq(0f, 0f)
    }

}
