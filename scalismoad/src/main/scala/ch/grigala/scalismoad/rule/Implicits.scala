package ch.grigala.scalismoad.rule

import ch.grigala.scalismoad.value.{ContainerValue, NonContainerValue, Value}


object Implicits {

    implicit class ValueOps[U[_], T](val self: U[T]) extends AnyVal {
        type C = ContainerValue[U, T]
        type NC = NonContainerValue[U, T]

        def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
            rhs match {
                case r: NC => ContainerValue[U, T](vr.addCM(self, r.data))
                case r: C => ContainerValue[U, T](vr.addCC(self, r.data))
            }
        }

        def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
            rhs match {
                case r: NC => ContainerValue[U, T](vr.subCM(self, r.data))
                case r: C => ContainerValue[U, T](vr.subCC(self, r.data))
            }
        }

        def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
            rhs match {
                case r: NC => ContainerValue[U, T](vr.mulCM(self, r.data))
                case r: C => ContainerValue[U, T](vr.mulCC(self, r.data))
            }
        }

        def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
            rhs match {
                case r: NC => ContainerValue[U, T](vr.divCM(self, r.data))
                case r: C => ContainerValue[U, T](vr.divCC(self, r.data))
            }
        }

        def unary_+(implicit vr: ValueRule[U, T]): U[T] = vr.posC(self)

        def unary_-(implicit vr: ValueRule[U, T]): U[T] = vr.negC(self)

    }

}
