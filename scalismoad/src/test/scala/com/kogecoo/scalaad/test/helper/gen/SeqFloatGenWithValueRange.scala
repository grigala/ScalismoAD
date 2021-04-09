package com.kogecoo.scalaad.test.helper.gen

import ch.grigala.ad.graph._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule
import ch.grigala.ad.value.{ContainerValue, NonContainerValue}
import org.scalacheck.Gen


class SeqFloatNodeGenWithValueRange(min: Float = -1e15f, max: Float = 1e15f) extends GenNode[Seq, Float] {

    implicit private[this] val r = new SeqFloatValueRule

    override def genVarWithSource(restrict: Float => Boolean): Gen[VarSample[Seq, Float]] = {
        for {
            v <- Gen.containerOf[Seq, Float](Gen.choose(min, max)) suchThat { a => a.nonEmpty && a.forall(restrict) }
        } yield new VarSample[Seq, Float](Var[Seq, Float](v), v)
    }

    override def genScalarConstWithSource(restrict: Float => Boolean): Gen[ScalarConstSample[Seq, Float]] = {
        for {
            v <- Gen.choose(min, max) suchThat restrict
        } yield new ScalarConstSample[Seq, Float](ScalarConst[Seq, Float](v), v)
    }

    override def genContainerConstWithSource(restrict: Float => Boolean): Gen[ContainerConstSample[Seq, Float]] = {
        for {
            v <- Gen.containerOf[Seq, Float](Gen.choose(min, max)) suchThat { a => a.nonEmpty && a.forall(restrict) }
        } yield new ContainerConstSample[Seq, Float](ContainerConst[Seq, Float](v), v)
    }

}


class SeqFloatValueGenWithValueRange(min: Float = -1e15f, max: Float = 1e15f) extends GenValue[Seq, Float] {

    implicit private[this] val r = new SeqFloatValueRule

    override def genNonContainerValueWithSource(restrict: Float => Boolean): Gen[NonContainerValueSample[Seq, Float]] = {
        for {
            v <- Gen.choose(min, max) suchThat restrict
        } yield new NonContainerValueSample[Seq, Float](NonContainerValue[Seq, Float](v), v)
    }

    override def genContainerValueWithSource(restrict: Float => Boolean): Gen[ContainerValueSample[Seq, Float]] = {
        for {
            v <- Gen.containerOf[Seq, Float](Gen.choose(min, max)) suchThat { a => a.nonEmpty && a.forall(restrict) }
        } yield new ContainerValueSample[Seq, Float](ContainerValue[Seq, Float](v), v)
    }

}
