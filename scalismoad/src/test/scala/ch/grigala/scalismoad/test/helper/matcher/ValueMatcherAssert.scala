package ch.grigala.scalismoad.test.helper.matcher

import ch.grigala.scalismoad.value.Value
import ch.grigala.scalismoad.test.helper.rule.CompareRule


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
