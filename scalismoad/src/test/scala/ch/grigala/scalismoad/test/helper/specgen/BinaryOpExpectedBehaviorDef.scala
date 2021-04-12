package ch.grigala.scalismoad.test.helper.specgen

import ch.grigala.scalismoad.graph.Node
import ch.grigala.scalismoad.rule.ValueRule


abstract class BinaryOpExpectedBehaviorDef[U[_], T](implicit vr: ValueRule[U, T]) {

    val zero: T

    def zero(shape: U[T]): U[T]


    def op(a: Node[U, T], b: Node[U, T]): Node[U, T]


    def applyScalarScalar(a: T, b: T): T

    def applyScalarContainer(a: T, b: U[T]): U[T]

    def applyScalarVar(a: T, b: U[T]): U[T]


    def applyContainerScalar(a: U[T], b: T): U[T]

    def applyContainerContainer(a: U[T], b: U[T]): U[T]

    def applyContainerVar(a: U[T], b: U[T]): U[T]


    def applyVarScalar(a: U[T], b: T): U[T]

    def applyVarContainer(a: U[T], b: U[T]): U[T]

    def applyVarVar(a: U[T], b: U[T]): U[T]


    def derivScalarVarWrtRight(a: T, b: U[T]): U[T]

    def derivScalarVarWrtUnknown(a: T, b: U[T]): U[T] = zero(b)

    def derivContainerVarWrtRight(a: U[T], b: U[T]): U[T]

    def derivContainerVarWrtUnknown(a: U[T], b: U[T]): U[T] = zero(a)

    def derivVarScalarWrtLeft(a: U[T], b: T): U[T]

    def derivVarScalarWrtUnknown(a: U[T], b: T): U[T] = zero(a)

    def derivVarContainerWrtLeft(a: U[T], b: U[T]): U[T]

    def derivVarContainerWrtUnknown(a: U[T], b: U[T]): U[T] = zero(a)

    def derivVarVarWrtLeft(a: U[T], b: U[T]): U[T]

    def derivVarVarWrtRight(a: U[T], b: U[T]): U[T]

    def derivVarVarWrtUnknown(a: U[T], b: U[T]): U[T] = zero(a)

    def derivVarVarWrtSelf(a: U[T]): U[T]


    def propagateScalarScalarWithNCValue(a: T, b: T, c: T): T = zero

    def propagateScalarContainerWithNCValue(a: T, b: U[T], c: T): U[T] = zero(b)

    def propagateScalarVarWithNCValue(a: T, b: U[T], c: T): U[T]


    def propagateContainerScalarWithNCValue(a: U[T], b: T, c: T): U[T] = zero(a)

    def propagateContainerContainerWithNCValue(a: U[T], b: U[T], c: T): U[T] = zero(a)

    def propagateContainerVarWithNCValue(a: U[T], b: U[T], c: T): U[T]


    def propagateVarScalarWithNCValue(a: U[T], b: T, c: T): U[T]

    def propagateVarContainerWithNCValue(a: U[T], b: U[T], c: T): U[T]

    def propagateVarVarWithNCValue(a: U[T], b: U[T], c: T): U[T]


    def propagateScalarScalarWithCValue(a: T, b: T, c: U[T]): U[T] = zero(c)

    def propagateScalarContainerWithCValue(a: T, b: U[T], c: U[T]): U[T] = zero(b)

    def propagateScalarVarWithCValue(a: T, b: U[T], c: U[T]): U[T]


    def propagateContainerScalarWithCValue(a: U[T], b: T, c: U[T]): U[T] = zero(a)

    def propagateContainerContainerWithCValue(a: U[T], b: U[T], c: U[T]): U[T] = zero(a)

    def propagateContainerVarWithCValue(a: U[T], b: U[T], c: U[T]): U[T]


    def propagateVarScalarWithCValue(a: U[T], b: T, c: U[T]): U[T]

    def propagateVarContainerWithCValue(a: U[T], b: U[T], c: U[T]): U[T]

    def propagateVarVarWithCValue(a: U[T], b: U[T], c: U[T]): U[T]


    def gradScalarScalar(a: T, b: T): T = zero

    def gradScalarContainer(a: T, b: U[T]): U[T] = zero(b)

    def gradScalarVar(a: T, b: U[T]): U[T]

    def gradContainerScalar(a: U[T], b: T): U[T] = zero(a)

    def gradContainerContainer(a: U[T], b: U[T]): U[T] = zero(a)

    def gradContainerVar(a: U[T], b: U[T]): U[T]

    def gradVarScalar(a: U[T], b: T): U[T]

    def gradVarContainer(a: U[T], b: U[T]): U[T]

    def gradVarVar(a: U[T], b: U[T]): U[T]

}
