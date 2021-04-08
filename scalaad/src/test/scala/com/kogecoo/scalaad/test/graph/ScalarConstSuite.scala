package com.kogecoo.scalaad.test.graph

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherAssert._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatExactCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue}
import org.scalatest.funsuite.AnyFunSuite


class ScalarConstSuite extends AnyFunSuite {


  implicit val seqFloatCompareRule = new SeqFloatExactCompareRule

  test("ScalarConst - Seq[Float]") {

    val var12_3 = Var[Seq, Float](Seq(12.0f, 3f))
    val c45_6 = ContainerConst[Seq, Float](Seq(45.0f, 6.0f))
    val sc7 = ScalarConst(7.0f)

    val value89 = NonContainerValue[Seq, Float](89.0f)
    val cValue10_11 = ContainerValue[Seq, Float](Seq(10.0f, 11.0f))

    val a1 = ScalarConst[Seq, Float](3.1f)

    a1.apply() shouldBe 3.1f
    a1.deriv(var12_3) shouldBe Seq(0f, 0f)
    a1.propagate(value89) shouldBe 0f
    a1.propagate(cValue10_11) shouldBe Seq(0.0f, 0.0f)

  }

}
