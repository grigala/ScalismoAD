package ch.grigala.scalismoad.value

import ch.grigala.scalismoad.graph.Scalar
import ch.grigala.scalismoad.rule.ValueRule

case class NonContainerValue[U[_], T](data: T) extends Value[U, T] {

    type C = ContainerValue[U, T]
    type NC = NonContainerValue[U, T]

    def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.addMS(data, r.data))
            case r: NC => NonContainerValue[U, T](vr.addMM(data, r.data))
        }
    }

    def +(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.addMS(data, rhs))
    }

    def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.subMS(data, r.data))
            case r: NC => NonContainerValue[U, T](vr.subMM(data, r.data))
        }
    }

    def -(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.subMS(data, rhs))
    }

    def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.mulMS(data, r.data))
            case r: NC => NonContainerValue[U, T](vr.mulMM(data, r.data))
        }
    }

    def *(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.mulMS(data, rhs))
    }

    def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.divMS(data, r.data))
            case r: NC => NonContainerValue[U, T](vr.divMM(data, r.data))
        }
    }

    def /(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.mulMS(data, rhs))
    }

    def <(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.ltMS(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.ltMM(data, r.data))
        }
    }

    def <(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.ltMS(data, rhs))
    }

    def <=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.lteMS(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.lteMM(data, r.data))
        }
    }

    def <=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.lteMS(data, rhs))
    }

    def >(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.gtMS(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.gtMM(data, r.data))
        }
    }

    def >(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.gtMS(data, rhs))
    }

    def >=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.gteMS(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.gteMM(data, r.data))
        }
    }

    def >=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.gteMS(data, rhs))
    }

    def ==(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.eqMS(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.eqMM(data, r.data))
        }
    }

    def ==(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.eqMS(data, rhs))
    }

    def close(rhs: Value[U, T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.closeMS(data, r.data, eps))
            case r: NC => NonContainerValue[U, Boolean](vr.closeMM(data, r.data, eps))
        }
    }

    def close(rhs: U[T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.closeMS(data, rhs, eps))
    }

    def unary_+(implicit vr: ValueRule[U, T]): Value[U, T] = NonContainerValue[U, T](vr.posM(data))

    def unary_-(implicit vr: ValueRule[U, T]): Value[U, T] = NonContainerValue[U, T](vr.negM(data))

    def T()(implicit vr: ValueRule[U, T]): Value[U, T] = NonContainerValue[U, T](vr.transposeM(data))

    override def unwrapContainerValue: U[T] = ???

    override def unwrapNonContainerValue: T = data
}


