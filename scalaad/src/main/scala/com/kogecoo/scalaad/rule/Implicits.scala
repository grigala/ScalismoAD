package com.kogecoo.scalaad.rule

import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue, Value}




object Implicits {

    implicit class ValueOps[U[_], T](val self: U[T]) extends AnyVal {
        type C = ContainerValue[U, T]
        type NC = NonContainerValue[U, T]

        def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
            rhs match {
                case r: NC => ContainerValue[U, T](vr.addSM(self, r.data))
                case r: C => ContainerValue[U, T](vr.addSS(self, r.data))
            }
        }

        def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
            rhs match {
                case r: NC => ContainerValue[U, T](vr.subSM(self, r.data))
                case r: C => ContainerValue[U, T](vr.subSS(self, r.data))
            }
        }

        def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
            rhs match {
                case r: NC => ContainerValue[U, T](vr.mulSM(self, r.data))
                case r: C => ContainerValue[U, T](vr.mulSS(self, r.data))
            }
        }

        def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
            rhs match {
                case r: NC => ContainerValue[U, T](vr.divSM(self, r.data))
                case r: C => ContainerValue[U, T](vr.divSS(self, r.data))
            }
        }

        def unary_+(implicit vr: ValueRule[U, T]): U[T] = vr.posS(self)

        def unary_-(implicit vr: ValueRule[U, T]): U[T] = vr.negS(self)

    }

}
