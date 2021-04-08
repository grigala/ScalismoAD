package com.kogecoo.scalaad.rule

trait ValueWrapperRule[Wrappee, Wrapper[_], T] {
    def toWrapper(data: Wrappee): Wrapper[T]
}
