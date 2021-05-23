package ch.grigala.scalismoad.rule

import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue, Value}

// for interactions between (DenseMatrix[x], DenseVector[x], x=Double)
trait ValueRuleBreeze[M[_], V[_], T] {

    //    final def zero(shape: Value[_, T]): Value[_, T] = shape match {
    //        case _: NonContainerValue[U, T] => zero
    //        case s1: ContainerValue[U, T] => zeroS1(s1.data)
    //        case s2: ContainerValue[V, T] => zeroS2(s2.data)
    //    }

    //    // DenseMatrix[Double] -> M[T]
    //    final def zero(shape: M[T]): Value[M, T] = toValue(zeroS1(shape))
    //    final def toValue(v: M[T])(implicit e: DummyImplicit): Value[M, T] = ContainerValue[M, T](v)
    //    def zeroS1(shape: M[T]): M[T]
    //    def oneS1(shape: M[T]): M[T]
    //
    //    // DenseVector[Double] -> V[T]
    //    final def zero(shape: V[T]): Value[V, T] = toValue(zeroS2(shape))
    //    final def toValue(v: V[T])(implicit e: DummyImplicit): Value[V, T] = ContainerValue[V, T](v)
    //    def zeroS2(shape: V[T]): V[T]
    //    def oneS2(shape: V[T]): V[T]
    //
    //    // Double
    //    final def zero: Value[M, T] = toValue(zeroM)
    //    final def toValue(v: T): Value[M, T] = NonContainerValue[M, T](v)
    //    def zeroM: T
    //    def oneM: T

    // + operation for every combination of DenseMatrix, DenseVector, Double
    def addMM(l: M[T], r: M[T]): M[T]

    def addMV(l: M[T], r: V[T]): M[T]

    def addMD(l: M[T], r: T): M[T]

    def addVV(l: V[T], r: V[T]): V[T]

    def addVD(l: V[T], r: T): V[T]

    def addDM(l: T, r: M[T]): M[T]

    def addDV(l: T, r: V[T]): V[T]

    def addDD(l: T, r: T): T

    def subMM(l: M[T], r: M[T]): M[T]

    def subMV(l: M[T], r: V[T]): M[T]

    def subMD(l: M[T], r: T): M[T]

    def subVV(l: V[T], r: V[T]): V[T]

    def subVD(l: V[T], r: T): V[T]

    def subDM(l: T, r: M[T]): M[T]

    def subDV(l: T, r: V[T]): V[T]

    def subDD(l: T, r: T): T

    def mulMM(l: M[T], r: M[T]): M[T]

    def mulMV(l: M[T], r: V[T]): M[T]

    def mulMD(l: M[T], r: T): M[T]

    def mulVV(l: V[T], r: V[T]): V[T]

    def mulVD(l: V[T], r: T): V[T]

    def mulDM(l: T, r: M[T]): M[T]

    def mulDV(l: T, r: V[T]): V[T]

    def mulDD(l: T, r: T): T

    def divMM(l: M[T], r: M[T]): M[T]

    def divMV(l: M[T], r: V[T]): M[T]

    def divMD(l: M[T], r: T): M[T]

    def divVV(l: V[T], r: V[T]): V[T]

    def divVD(l: V[T], r: T): V[T]

    def divDM(l: T, r: M[T]): M[T]

    def divDV(l: T, r: V[T]): V[T]

    def divDD(l: T, r: T): T
}
