package ch.grigala.scalismoad.test.graph

import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.test.helper.matcher.ValueMatcherAssert._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatExactCompareRule
import org.scalatest.funsuite.AnyFunSuite


class VarSuite extends AnyFunSuite {

    implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

  test("Var - Seq[Float]") {

    val var12_3 = Var[Seq, Float](Seq(12.0f, 3f))
    val c45_6 = ContainerConst[Seq, Float](Seq(45.0f, 6.0f))
    val sc7 = ScalarConst(7.0f)

    val value89 = NonContainerValue[Seq, Float](89.0f)
    val cValue10_11 = ContainerValue[Seq, Float](Seq(10.0f, 11.0f))

    val a1 = Var(Seq(3.1f, 41.5f))

    a1.apply() shouldBe Seq(3.1f, 41.5f)
    a1.deriv(a1) shouldBe Seq(1f, 1f)
    a1.deriv(var12_3) shouldBe Seq(0f, 0f)
    a1.propagate(value89) shouldBe Seq(89f, 89f)
    a1.propagate(cValue10_11) shouldBe Seq(10f, 11f)

  }

}
