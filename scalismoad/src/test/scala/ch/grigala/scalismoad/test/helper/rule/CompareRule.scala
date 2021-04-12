package ch.grigala.scalismoad.test.helper.rule

abstract class CompareRule[U[_], T] {

    def shouldBe(a: U[T], b: U[T])(implicit d: DummyImplicit): Boolean

    def shouldBe(a: T, b: T): Boolean

}
