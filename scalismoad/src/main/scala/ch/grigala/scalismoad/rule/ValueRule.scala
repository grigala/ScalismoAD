package ch.grigala.scalismoad.rule

import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue, Value}


// Define concrete calculations for U[T] type instances which performed on nodes in computational graph.
trait ValueRule[U[_], T] {

    final def zero(shape: Value[U, T]): Value[U, T] = shape match {
        case _: NonContainerValue[U, T] => zero
        case s: ContainerValue[U, T] => zero(s.data)
    }

    final def zero: Value[U, T] = toValue(zeroM)

    final def toValue(v: T): Value[U, T] = NonContainerValue[U, T](v)

    final def zero(shape: U[T]): Value[U, T] = toValue(zeroC(shape))

    final def toValue(v: U[T])(implicit e: DummyImplicit): Value[U, T] = ContainerValue[U, T](v)

    final def one(shape: Value[U, T]): Value[U, T] = shape match {
        case _: NonContainerValue[U, T] => one
        case s: ContainerValue[U, T] => one(s.data)
    }

    final def one(implicit d: DummyImplicit): Value[U, T] = toValue(oneM)

    final def one(shape: U[T])(implicit d: DummyImplicit): Value[U, T] = toValue(oneC(shape))

    def zeroM: T

    def zeroC(shape: U[T]): U[T]

    def oneM: T

    def oneC(shape: U[T]): U[T]

    // binary ops between Container and Container, hence CC
    def addCC(l: U[T], r: U[T]): U[T]

    def subCC(l: U[T], r: U[T]): U[T]

    def mulCC(l: U[T], r: U[T]): U[T]

    def divCC(l: U[T], r: U[T]): U[T]

    // binary ops for Container and Member object i.e. Double
    // here we don't assume the commutative law for add/mul.
    def addCM(l: U[T], r: T): U[T]

    def subCM(l: U[T], r: T): U[T]

    def mulCM(l: U[T], r: T): U[T]

    def divCM(l: U[T], r: T): U[T]

    // binary ops for Member and Container
    def addMC(l: T, r: U[T]): U[T]

    def subMC(l: T, r: U[T]): U[T]

    def mulMC(l: T, r: U[T]): U[T]

    def divMC(l: T, r: U[T]): U[T]

    // binary ops for memeber and member
    def addMM(l: T, r: T): T

    def subMM(l: T, r: T): T

    def mulMM(l: T, r: T): T

    def divMM(l: T, r: T): T

    def ltCC(l: U[T], r: U[T]): U[Boolean]

    def lteCC(l: U[T], r: U[T]): U[Boolean]

    def gtCC(l: U[T], r: U[T]): U[Boolean]

    def gteCC(l: U[T], r: U[T]): U[Boolean]

    def eqCC(l: U[T], r: U[T]): U[Boolean]

    def ltCM(l: U[T], r: T): U[Boolean]

    def lteCM(l: U[T], r: T): U[Boolean]

    def gtCM(l: U[T], r: T): U[Boolean]

    def gteCM(l: U[T], r: T): U[Boolean]

    def eqCM(l: U[T], r: T): U[Boolean]

    def ltMC(l: T, r: U[T]): U[Boolean]

    def lteMC(l: T, r: U[T]): U[Boolean]

    def gtMC(l: T, r: U[T]): U[Boolean]

    def gteMC(l: T, r: U[T]): U[Boolean]

    def eqMC(l: T, r: U[T]): U[Boolean]

    def ltMM(l: T, r: T): Boolean

    def lteMM(l: T, r: T): Boolean

    def gtMM(l: T, r: T): Boolean

    def gteMM(l: T, r: T): Boolean

    def eqMM(l: T, r: T): Boolean

    def posC(v: U[T]): U[T]

    def negC(v: U[T]): U[T]

    def posM(v: T): T

    def negM(v: T): T

    def transposeC(v: U[T]): U[T]

    def transposeM(v: T): T

    def closeCC(l: U[T], r: U[T], eps: T): U[Boolean]

    def closeCM(l: U[T], r: T, eps: T): U[Boolean]

    def closeMC(l: T, r: U[T], eps: T): U[Boolean]

    def closeMM(l: T, r: T, eps: T): Boolean

    def whereCCC(cond: U[Boolean], a: U[T], b: U[T]): U[T]

    def whereCCM(cond: U[Boolean], a: U[T], b: T): U[T]

    def whereCMC(cond: U[Boolean], a: T, b: U[T]): U[T]

    def whereCMM(cond: U[Boolean], a: T, b: T): U[T]

    def whereMCC(cond: Boolean, a: U[T], b: U[T]): U[T]

    def whereMCM(cond: Boolean, a: U[T], b: T): U[T]

    def whereMMC(cond: Boolean, a: T, b: U[T]): U[T]

    def whereMMM(cond: Boolean, a: T, b: T): T

}

