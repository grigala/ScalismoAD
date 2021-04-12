package ch.grigala.scalismoad.test.graph.math.suite

import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.test.helper.matcher.ValueMatcherAssert._
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule.Implicits._
import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatSoftCompareRule
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random


class CosSuite extends AnyFunSuite {

    implicit val seqFloatCompareRule = new SeqFloatSoftCompareRule

    test("Cos - Seq[Float]") {

        val n = 10
        val varSeq1 = Seq.fill[Float](n)(Random.nextFloat())
        val varSeq2 = Seq.fill[Float](n)(Random.nextFloat())
        val ccSeq = Seq.fill[Float](n)(Random.nextFloat())
        val sc = Random.nextFloat()
        val value = Random.nextFloat()
        val cValue = Seq.fill(n)(Random.nextFloat())

        val varNode1 = Var[Seq, Float](varSeq1)
        val varNode2 = Var[Seq, Float](varSeq2)
        val ccNode = ContainerConst[Seq, Float](ccSeq)
        val scNode = ScalarConst(sc)

        val valueRand = NonContainerValue[Seq, Float](value)
        val cValueRand = ContainerValue[Seq, Float](cValue)

        (0 to 100).foreach { i =>
            // ScalarConst
            val scCos = cos(ccNode)

            scCos.apply() shouldBe ccSeq.map(scala.math.cos(_).toFloat)
            scCos.deriv(varNode1) shouldBe Seq.fill(n)(0f)
            scCos.propagate(valueRand) shouldBe Seq.fill(n)(0f)
            scCos.propagate(cValueRand) shouldBe Seq.fill(n)(0f)

            // ContainerConst
            val ccCos = cos(ccNode)

            ccCos.apply() shouldBe ccSeq.map(scala.math.cos(_).toFloat)
            ccCos.deriv(varNode1) shouldBe Seq.fill(n)(0f)
            ccCos.propagate(valueRand) shouldBe Seq.fill(n)(0f)
            ccCos.propagate(cValueRand) shouldBe Seq.fill(n)(0f)

            // Var
            val varCos = cos(varNode1)

            varCos.apply() shouldBe varSeq1.map(scala.math.cos(_).toFloat)
            varCos.deriv(varNode1) shouldBe varSeq1.map { v => (-scala.math.sin(v)).toFloat }
            varCos.deriv(varNode2) shouldBe Seq.fill(n)(0f)
            varCos.propagate(valueRand) shouldBe varSeq1.map { v => (-value * scala.math.sin(v)).toFloat }
            varCos.propagate(cValueRand) shouldBe varSeq1.map({ v => -scala.math.sin(v) }).zip(cValue).map { case (a, b) => (a * b).toFloat }

        }

    }

}
