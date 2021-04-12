package ch.grigala.scalismoad.test.helper.gen

import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue}
import ch.grigala.scalismoad.test.helper.rule.SeqFloatValueRule
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen


class SeqFloatNodeGen extends GenNode[Seq, Float] {

    implicit private[this] val r = new SeqFloatValueRule

    override def genVarWithSource(restrict: Float => Boolean): Gen[VarSample[Seq, Float]] = {
        for {
            v <- arbitrary[Seq[Float]] suchThat { a => a.nonEmpty && a.forall(restrict) }
        } yield new VarSample[Seq, Float](Var[Seq, Float](v), v)
    }

    override def genScalarConstWithSource(restrict: Float => Boolean): Gen[ScalarConstSample[Seq, Float]] = {
        for {
            v <- arbitrary[Float] suchThat restrict
        } yield new ScalarConstSample[Seq, Float](ScalarConst[Seq, Float](v), v)
    }

    override def genContainerConstWithSource(restrict: Float => Boolean): Gen[ContainerConstSample[Seq, Float]] = {
        for {
            v <- arbitrary[Seq[Float]] suchThat { a => a.nonEmpty && a.forall(restrict) }
        } yield new ContainerConstSample[Seq, Float](ContainerConst[Seq, Float](v), v)
    }

}


class SeqFloatValueGen extends GenValue[Seq, Float] {

    implicit private[this] val r = new SeqFloatValueRule

    override def genNonContainerValueWithSource(restrict: Float => Boolean): Gen[NonContainerValueSample[Seq, Float]] = {
        for {
            v <- arbitrary[Float] suchThat restrict
        } yield new NonContainerValueSample[Seq, Float](NonContainerValue[Seq, Float](v), v)
    }

    override def genContainerValueWithSource(restrict: Float => Boolean): Gen[ContainerValueSample[Seq, Float]] = {
        for {
            v <- arbitrary[Seq[Float]] suchThat { a => a.nonEmpty && a.forall(restrict) }
        } yield new ContainerValueSample[Seq, Float](ContainerValue[Seq, Float](v), v)
    }

}

