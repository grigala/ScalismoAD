package com.kogecoo.scalaad.test.helper.specgen

import com.kogecoo.scalaad.graph.Node
import com.kogecoo.scalaad.rule.ValueRule




abstract class UnaryOpExpectedBehaviorDef[U[_], T](implicit vr: ValueRule[U, T]) {

    val zero: T

    def zero(shape: U[T]): U[T]

    def op(a: Node[U, T]): Node[U, T]


    def applyScalar(a: T): T

    def applyContainer(a: U[T]): U[T]

    def applyVar(a: U[T]): U[T]


    def derivScalarWrtSelf(a: T): T = zero

    def derivContainerWrtSelf(a: U[T]): U[T] = zero(a)

    def derivVarWrtSelf(a: U[T]): U[T]


    def derivScalarWrtUnknown(a: T, b: U[T]): U[T] = zero(b)

    def derivContainerWrtUnknown(a: U[T], b: U[T]): U[T] = zero(b)

    def derivVarWrtUnknown(a: U[T], b: U[T]): U[T] = zero(b)


    def propagateScalarWithNCValue(a: T, b: T): T = zero

    def propagateContainerWithNCValue(a: U[T], b: T): U[T] = zero(a)

    def propagateVarWithNCValue(a: U[T], b: T): U[T]


    def propagateScalarWithCValue(a: T, b: U[T]): U[T] = zero(b)

    def propagateContainerWithCValue(a: U[T], b: U[T]): U[T] = zero(a)

    def propagateVarWithCValue(a: U[T], b: U[T]): U[T]


    def gradScalar(a: T): T = zero

    def gradContainer(a: U[T]): U[T] = zero(a)

    def gradVar(a: U[T]): U[T]


}
