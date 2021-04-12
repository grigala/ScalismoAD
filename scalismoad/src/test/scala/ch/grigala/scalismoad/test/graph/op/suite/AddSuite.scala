package ch.grigala.scalismoad.test.graph.op.suite

import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.test.helper.matcher.ValueMatcherAssert._
import ch.grigala.scalismoad.test.helper.rule.ScalarIntValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.test.helper.rule.ScalarIntCompareRule
import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue}
import ch.grigala.scalismoad.test.helper.rule.{ScalarIntCompareRule, SeqFloatExactCompareRule}
import org.scalatest.funsuite.AnyFunSuite


class AddSuite extends AnyFunSuite {

    implicit val scalarIntCompareRule = new ScalarIntCompareRule

    implicit val seqFloatCompareRule = new SeqFloatExactCompareRule


    test("Add - Scalar[Int]") {

        val value42 = NonContainerValue[Scalar, Int](42)
        val cValue211 = ContainerValue[Scalar, Int](Scalar(211))

        val sc4 = ScalarConst(4)
        val sc3 = ScalarConst(3)
        val cc31 = ContainerConst[Scalar, Int](Scalar(31))
        val cc72 = ContainerConst[Scalar, Int](Scalar(72))
        val var7 = Var(Scalar(7))
        val var13 = Var(Scalar(13))
        val var99 = Var(Scalar(99))

        // ScalarConst + ScalarConst
        val a1 = Add(sc4, sc3)

        a1.apply() shouldBe 7
        a1.deriv(var7) shouldBe Scalar(0)
        a1.deriv(var13) shouldBe Scalar(0)
        a1.propagate(value42) shouldBe 0
        a1.propagate(cValue211) shouldBe Scalar(0)

        // ScalarConst + ContainerConst
        val a2 = Add(sc4, cc72)

        a2.apply() shouldBe Scalar(76)
        a2.deriv(var7) shouldBe Scalar(0)
        a2.deriv(var13) shouldBe Scalar(0)
        a2.propagate(value42) shouldBe Scalar(0)
        a2.propagate(cValue211) shouldBe Scalar(0)

        // ContaoinerConst + ScalarConst
        val a3 = Add(cc31, sc3)

        a3.apply() shouldBe Scalar(34)
        a3.deriv(var7) shouldBe Scalar(0)
        a3.deriv(var13) shouldBe Scalar(0)
        a3.propagate(value42) shouldBe Scalar(0)
        a3.propagate(cValue211) shouldBe Scalar(0)

        // ContainerConst + ContainerConst
        val a4 = Add(cc31, cc72)

        a4.apply() shouldBe Scalar(103)
        a4.deriv(var7) shouldBe Scalar(0)
        a4.deriv(var13) shouldBe Scalar(0)
        a4.propagate(value42) shouldBe Scalar(0)
        a4.propagate(cValue211) shouldBe Scalar(0)

        // above a1 - a4 is may be redundant

        // Var + ScalarConst
        val a5 = Add(var7, sc3)

        a5.apply() shouldBe Scalar(10)
        a5.deriv(var7) shouldBe Scalar(1)
        a5.deriv(var99) shouldBe Scalar(0)
        a5.propagate(value42) shouldBe Scalar(42)
        a5.propagate(cValue211) shouldBe Scalar(211)

        // Var + ContainerConst
        val a6 = Add(var7, cc31)

        a6.apply() shouldBe Scalar(7 + 31)
        a6.deriv(var7) shouldBe Scalar(1)
        a6.deriv(var99) shouldBe Scalar(0)
        a6.propagate(value42) shouldBe Scalar(42)
        a6.propagate(cValue211) shouldBe Scalar(211)

        // ScalarConst + Var
        val a7 = Add(sc4, var13)

        a7.apply() shouldBe Scalar(17)
        a7.deriv(var99) shouldBe Scalar(0)
        a7.deriv(var13) shouldBe Scalar(1)
        a7.propagate(value42) shouldBe Scalar(42)
        a7.propagate(cValue211) shouldBe Scalar(211)

        // Var + ContainerConst
        val a8 = Add(cc31, var7)

        a6.apply() shouldBe Scalar(7 + 31)
        a6.deriv(var7) shouldBe Scalar(1)
        a6.deriv(var99) shouldBe Scalar(0)
        a6.propagate(value42) shouldBe Scalar(42)
        a6.propagate(cValue211) shouldBe Scalar(211)

        // Var + Var
        val a9 = Add(var7, var13)

        a9.apply() shouldBe Scalar(20)
        a9.deriv(var7) shouldBe Scalar(1)
        a9.deriv(var13) shouldBe Scalar(1)
        a9.deriv(var99) shouldBe Scalar(0)
        a9.propagate(value42) shouldBe Scalar(84)
        a9.propagate(cValue211) shouldBe Scalar(422)

        // more complex one
        // ((x + 3) + y), x = 7, y = 13
        val a10 = Add(Add(var7, sc3), var13)

        a10.apply() shouldBe Scalar(23)
        a10.deriv(var7) shouldBe Scalar(1)
        a10.deriv(var13) shouldBe Scalar(1)
        a10.deriv(var99) shouldBe Scalar(0)
        a10.propagate(value42) shouldBe Scalar(84)
        a10.propagate(cValue211) shouldBe Scalar(422)
    }

    test("Add - Seq[Float]") {

        val var54_7 = Var(Seq(54.0f, 7f))
        val var71_3 = Var(Seq(71.0f, 3f))
        val var33_51 = Var(Seq(33.0f, 51.0f))
        val c12_42 = ContainerConst[Seq, Float](Seq(12.0f, 42.0f))
        val c63_21 = ContainerConst[Seq, Float](Seq(63.0f, 21.0f))
        val sc2 = ScalarConst[Seq, Float](2.0f)
        val sc5 = ScalarConst[Seq, Float](5.0f)

        val value15 = NonContainerValue[Seq, Float](15.0f)
        val cValue8 = ContainerValue[Seq, Float](Seq(3.0f, 5.0f))

        // ContainerConst + ContainerConst
        val a1 = Add(c12_42, c63_21)

        a1.apply() shouldBe Seq(75.0f, 63.0f)
        a1.deriv(var54_7) shouldBe Seq(0f, 0f)
        a1.propagate(value15) shouldBe Seq(0f, 0f)
        a1.propagate(cValue8) shouldBe Seq(0.0f, 0.0f)

        // above a1 - a4 is may be redundant

        // Var + ContainerConst
        val a2 = Add(var71_3, c63_21)

        a2.apply() shouldBe Seq(134f, 24f)
        a2.deriv(var71_3) shouldBe Seq(1f, 1f)
        a1.deriv(var54_7) shouldBe Seq(0f, 0f)
        a2.propagate(value15) shouldBe Seq(15f, 15f)
        a2.propagate(cValue8) shouldBe Seq(3.0f, 5.0f)

        // ContainerConst + Var
        val a3 = Add(c12_42, var33_51)

        a3.apply() shouldBe Seq(45f, 93f)
        a1.deriv(var54_7) shouldBe Seq(0f, 0f)
        a3.deriv(var33_51) shouldBe Seq(1f, 1f)
        a3.propagate(value15) shouldBe Seq(15f, 15f)
        a3.propagate(cValue8) shouldBe Seq(3.0f, 5.0f)

        // Var + Var
        val a4 = Add(var71_3, var33_51)

        a4.apply() shouldBe Seq(104f, 54f)
        a4.deriv(var71_3) shouldBe Seq(1f, 1f)
        a4.deriv(var33_51) shouldBe Seq(1f, 1f)
        a3.deriv(var54_7) shouldBe Seq(0f, 0f)
        a4.propagate(value15) shouldBe Seq(30f, 30f)
        a4.propagate(cValue8) shouldBe Seq(6.0f, 10.0f)

        // more complex one
        // ((x + 2) + y), x = [71, 3], y = [33, 51]
        val a5 = Add(Add(var71_3, sc2), var33_51)
        a5.apply() shouldBe Seq(106f, 56f)
        a5.deriv(var71_3) shouldBe Seq(1f, 1f)
        a5.deriv(var33_51) shouldBe Seq(1f, 1f)
        a3.deriv(var54_7) shouldBe Seq(0f, 0f)
        a5.propagate(value15) shouldBe Seq(30f, 30f)
        a5.propagate(cValue8) shouldBe Seq(6.0f, 10.0f)
    }

}
