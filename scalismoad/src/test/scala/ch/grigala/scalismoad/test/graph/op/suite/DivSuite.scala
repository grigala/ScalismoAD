package ch.grigala.scalismoad.test.graph.op.suite

import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.test.helper.matcher.ValueMatcherAssert._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatExactCompareRule
import org.scalatest.funsuite.AnyFunSuite


class DivSuite extends AnyFunSuite {

    implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

    test("Div - Seq[Float]") {

        val var42_12 = Var[Seq, Float](Seq(42.0f, 12f))
        val var71_3 = Var[Seq, Float](Seq(71.0f, 3f))
        val var33_51 = Var[Seq, Float](Seq(33.0f, 51.0f))
        val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
        val c63_21 = ContainerConst[Seq, Float](Seq(63.0f, 21.0f))
        val sc2 = ScalarConst(2.0f)

        val value15 = NonContainerValue[Seq, Float](15.0f)
        val cValue3_5 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

        // ContainerConst / ContainerConst
        val a1 = Div(c12_42, c63_21)

        a1.apply() shouldBe Seq(12f / 63f, 42f / 21f)
        a1.deriv(var71_3) shouldBe Seq(0f, 0f)
        a1.propagate(value15) shouldBe Seq(0f, 0f)
        a1.propagate(cValue3_5) shouldBe Seq(0.0f, 0.0f)

        // Var / ScalarConst
        val a2 = Div(var71_3, c63_21)

        a2.apply() shouldBe Seq(71f / 63f, 3f / 21f)
        a2.deriv(var71_3) shouldBe Seq(1f / 63f, 1f / 21f)
        a2.deriv(var33_51) shouldBe Seq(0f, 0f)
        a2.propagate(value15) shouldBe Seq(15f * 1f / 63f, 15f * 1f / 21f)
        a2.propagate(cValue3_5) shouldBe Seq(3.0f * 1f / 63f, 5.0f * 1f / 21f)

        // ScalarConst / Var
        val a3 = Div(c12_42, var33_51)

        a3.apply() shouldBe Seq(12f / 33f, 42f / 51f)
        a3.deriv(var71_3) shouldBe Seq(0f, 0f)
        a3.deriv(var33_51) shouldBe Seq(-12f / 33f / 33f, -42f / 51f / 51f)
        a3.propagate(value15) shouldBe Seq(-15f * 12f / 33f / 33f, -15 * 42f / 51f / 51f)
        a3.propagate(cValue3_5) shouldBe Seq(-3f * 12f / 33f / 33f, -5f * 42f / 51f / 51f)

        // Var / Var
        val a4 = Div(var71_3, var33_51)

        a4.apply() shouldBe Seq(71f / 33f, 3f / 51f)
        a4.deriv(var42_12) shouldBe Seq(0f, 0f)
        a4.deriv(var71_3) shouldBe Seq(1f / 33f, 1f / 51f)
        a4.deriv(var33_51) shouldBe Seq(-71f / 33f / 33f, -3f / 51f / 51f)
        a4.propagate(value15) shouldBe Seq(-15f * 71f / 33f / 33f + 15f / 33f, -15f * 3f / 51f / 51f + 15f / 51f)
        a4.propagate(cValue3_5) shouldBe Seq(-3f * 71f / 33f / 33f + 3f / 33f, -5f * 3f / 51f / 51f + 5f / 51f)

        // more complex one
        // ((x / 2) / y), x = [71, 3], y = [33, 51]
        val a5 = Div(Div(var71_3, sc2), var33_51)
        a5.apply() shouldBe Seq(71f / 2f / 33f, 3f / 2f / 51f)
        a4.deriv(var42_12) shouldBe Seq(0f, 0f)
        a5.deriv(var71_3) shouldBe Seq(1f / 33f / 2f, 1f / 51f / 2f)
        a5.deriv(var33_51) shouldBe Seq(-71f / 2f / 33f / 33f, -3f / 2f / 51f / 51f)
        a5.propagate(value15) shouldBe Seq(-15f * 71f / 2f / 33f / 33f + 15f / 33f / 2f, -15f * 3f / 2f / 51f / 51f + 15f / 51f / 2f)
        a5.propagate(cValue3_5) shouldBe Seq(-3f * 71f / 2f / 33f / 33f + 3f / 33f / 2f, -5f * 3f / 2f / 51f / 51f + 5f / 51f / 2f)
    }

}
