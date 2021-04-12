package ch.grigala.scalismoad.test.helper.gen

import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue, Value}
import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf


abstract class GenValue[U[_], T] {

    lazy val defaultRestriction = (_: T) => true

    final def genValue(restrict: T => Boolean = defaultRestriction): Gen[Value[U, T]] = {
        oneOf(genNonContainerValue(restrict), genContainerValue(restrict))
    }

    final def genNonContainerValue(restrict: T => Boolean = defaultRestriction): Gen[NonContainerValue[U, T]] = {
        genNonContainerValueWithSource(restrict).map(_.value)
    }

    final def genContainerValue(restrict: T => Boolean = defaultRestriction): Gen[ContainerValue[U, T]] = {
        genContainerValueWithSource(restrict).map(_.value)
    }

    def genNonContainerValueWithSource(restrict: T => Boolean = defaultRestriction): Gen[NonContainerValueSample[U, T]]

    def genContainerValueWithSource(restrict: T => Boolean = defaultRestriction): Gen[ContainerValueSample[U, T]]

}

class NonContainerValueSample[U[_], T](val value: NonContainerValue[U, T], val src: T) {
    override def toString = s"NonContainerValueSample(${value}, ${src})"
}

class ContainerValueSample[U[_], T](val value: ContainerValue[U, T], val src: U[T]) {
    override def toString = s"ContainerValueSample(${value}, ${src})"
}
