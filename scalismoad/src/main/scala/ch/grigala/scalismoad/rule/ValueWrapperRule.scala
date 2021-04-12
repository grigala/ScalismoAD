package ch.grigala.scalismoad.rule

trait ValueWrapperRule[Wrappee, Wrapper[_], T] {
    def toWrapper(data: Wrappee): Wrapper[T]
    def toWrappee(data: Wrapper[_]): Wrappee
}
