package ch.grigala.scalismoad.test.graph.op.suite

import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.test.helper.matcher.ValueMatcherAssert._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatExactCompareRule
import org.scalatest.funsuite.AnyFunSuite


class PosSuite extends AnyFunSuite {

    implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

    test("Pos - Seq[Float]") {

        val var71_3 = Var[Seq, Float](Seq(71.0f, 3f))
        val var3_52 = Var[Seq, Float](Seq(3.0f, 52f))
        val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
        val sc2 = ScalarConst(2.0f)

        val value15 = NonContainerValue[Seq, Float](15.0f)
        val cValue3_5 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

        // +ContainerConst
        val a1 = Pos(c12_42)

        a1.apply() shouldBe Seq(12f, 42f)
        a1.deriv(var71_3) shouldBe Seq(0f, 0f)
        a1.propagate(value15) shouldBe Seq(0f, 0f)
        a1.propagate(cValue3_5) shouldBe Seq(0.0f, 0.0f)

        // +Var
        val a2 = Pos(var71_3)

        a2.apply() shouldBe Seq(71f, 3f)
        a2.deriv(var71_3) shouldBe Seq(1f, 1f)
        a2.deriv(var3_52) shouldBe Seq(0f, 0f)
        a2.propagate(value15) shouldBe Seq(15f, 15f)
        a2.propagate(cValue3_5) shouldBe Seq(3.0f, 5f)

    }

}
