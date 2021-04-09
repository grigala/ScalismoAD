package com.kogecoo.scalaad.test.helper.matcher

import com.kogecoo.scalaad.test.helper.rule.CompareRule
import ch.grigala.ad.value.Value


object ValueMatcherAssert {

    implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {

        def shouldBe(v: T)(implicit compare: CompareRule[U, T]): Unit = {
            assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
        }

        def shouldBe(v: U[T])(implicit d: DummyImplicit, compare: CompareRule[U, T]): Unit = {
            assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
        }

        def shouldBe(v: Value[U, T])(implicit compare: CompareRule[U, T]): Unit = {
            assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
        }
    }
}

object ValueMatcherDiagrammedAssert {

    implicit class ValueOpsForTest[U[_], T](val self: Value[U, T]) extends AnyVal {

        def shouldBe(v: T)(implicit compare: CompareRule[U, T]): Unit = {
            assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
        }

        def shouldBe(v: U[T])(implicit d: DummyImplicit, compare: CompareRule[U, T]): Unit = {
            assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
        }

        def shouldBe(v: Value[U, T])(implicit compare: CompareRule[U, T]): Unit = {
            assert(ValueMatcher.ValueOpsForTest(self).shouldBe(v))
        }
    }

}
