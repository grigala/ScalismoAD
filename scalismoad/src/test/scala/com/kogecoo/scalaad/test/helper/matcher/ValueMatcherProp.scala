package com.kogecoo.scalaad.test.helper.matcher

import com.kogecoo.scalaad.test.helper.rule.CompareRule
import ch.grigala.ad.value.{ContainerValue, NonContainerValue, Value}
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean


object ValueMatcherProp {

    implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {

        def shouldBe(v: T)(implicit compare: CompareRule[U, T]): Prop = self match {
            case a: NonContainerValue[U, T] => compare.shouldBe(a.data, v) :| s"${a.data} shouldBe ${v}"
            case _ => false :| s"expects NonContainerValue but ${self}"
        }

        def shouldBe(v: U[T])(implicit d: DummyImplicit, compare: CompareRule[U, T]): Prop = self match {
            case a: ContainerValue[U, T] => compare.shouldBe(a.data, v) :| s"${a.data} shouldBe ${v}"
            case _ => false :| s"expects ContaienrValue but ${self}"
        }

        def shouldBe(v: Value[U, T])(implicit compare: CompareRule[U, T]): Prop = (self, v) match {
            case (a: ContainerValue[U, T], b: ContainerValue[U, T]) => compare.shouldBe(a.data, b.data) :| s"expects ${a.data} closeTo ${b.data}"
            case (a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => compare.shouldBe(a.data, b.data) :| s"expects ${a.data} closeTo ${b.data}"
            case _ => false :| s"expects same Value type but ${self}, ${v}"
        }
    }

}
