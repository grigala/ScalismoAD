package com.kogecoo.scalaad.test.helper.gen

import ch.grigala.ad.graph._
import com.kogecoo.scalaad.test.helper.rule.ScalarIntValueRule
import ch.grigala.ad.value.{ContainerValue, NonContainerValue}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen


class ScalarIntNodeGen extends GenNode[Scalar, Int] {

    implicit private[this] val r = new ScalarIntValueRule


    override def genVarWithSource(restrict: Int => Boolean): Gen[VarSample[Scalar, Int]] = {
        for {
            v <- arbitrary[Int] suchThat restrict
        } yield new VarSample[Scalar, Int](Var[Scalar, Int](Scalar(v)), Scalar(v))
    }

    override def genScalarConstWithSource(restrict: Int => Boolean): Gen[ScalarConstSample[Scalar, Int]] = {
        for {
            v <- arbitrary[Int] suchThat restrict
        } yield new ScalarConstSample[Scalar, Int](ScalarConst[Scalar, Int](v), v)
    }

    override def genContainerConstWithSource(restrict: Int => Boolean): Gen[ContainerConstSample[Scalar, Int]] = {
        for {
            v <- arbitrary[Int] suchThat restrict
        } yield new ContainerConstSample[Scalar, Int](ContainerConst[Scalar, Int](Scalar(v)), Scalar[Int](v))
    }
}

class ScalarIntValueGen extends GenValue[Scalar, Int] {

    implicit private[this] val r = new ScalarIntValueRule


    override def genNonContainerValueWithSource(restrict: Int => Boolean): Gen[NonContainerValueSample[Scalar, Int]] = {
        for {
            v <- arbitrary[Int] suchThat restrict
        } yield new NonContainerValueSample(NonContainerValue[Scalar, Int](v), v)
    }

    override def genContainerValueWithSource(restrict: Int => Boolean): Gen[ContainerValueSample[Scalar, Int]] = {
        for {
            v <- arbitrary[Int] suchThat (restrict)
        } yield new ContainerValueSample[Scalar, Int](ContainerValue[Scalar, Int](Scalar(v)), Scalar(v))
    }

}
