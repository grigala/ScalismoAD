package ch.grigala.scalismoad.value

import ch.grigala.scalismoad.graph.Scalar
import ch.grigala.scalismoad.rule.ValueRule

case class NonContainerValue[U[_], T](data: T) extends Value[U, T] {

    type C = ContainerValue[U, T]
    type NC = NonContainerValue[U, T]

    def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.addMC(data, r.data))
            case r: NC => NonContainerValue[U, T](vr.addMM(data, r.data))
        }
    }

    def +(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.addMC(data, rhs))
    }

    def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.subMC(data, r.data))
            case r: NC => NonContainerValue[U, T](vr.subMM(data, r.data))
        }
    }

    def -(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.subMC(data, rhs))
    }

    def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.mulMC(data, r.data))
            case r: NC => NonContainerValue[U, T](vr.mulMM(data, r.data))
        }
    }

    def *(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.mulMC(data, rhs))
    }

    def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.divMC(data, r.data))
            case r: NC => NonContainerValue[U, T](vr.divMM(data, r.data))
        }
    }

    def /(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.mulMC(data, rhs))
    }

    def <(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.ltMC(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.ltMM(data, r.data))
        }
    }

    def <(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.ltMC(data, rhs))
    }

    def <=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.lteMC(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.lteMM(data, r.data))
        }
    }

    def <=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.lteMC(data, rhs))
    }

    def >(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.gtMC(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.gtMM(data, r.data))
        }
    }

    def >(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.gtMC(data, rhs))
    }

    def >=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.gteMC(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.gteMM(data, r.data))
        }
    }

    def >=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.gteMC(data, rhs))
    }

    def ==(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.eqMC(data, r.data))
            case r: NC => NonContainerValue[U, Boolean](vr.eqMM(data, r.data))
        }
    }

    def ==(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.eqMC(data, rhs))
    }

    def close(rhs: Value[U, T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.closeMC(data, r.data, eps))
            case r: NC => NonContainerValue[U, Boolean](vr.closeMM(data, r.data, eps))
        }
    }

    def close(rhs: U[T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.closeMC(data, rhs, eps))
    }

    def unary_+(implicit vr: ValueRule[U, T]): Value[U, T] = NonContainerValue[U, T](vr.posM(data))

    def unary_-(implicit vr: ValueRule[U, T]): Value[U, T] = NonContainerValue[U, T](vr.negM(data))

    def T()(implicit vr: ValueRule[U, T]): Value[U, T] = NonContainerValue[U, T](vr.transposeM(data))

    override def unwrap: Scalar[_] = Scalar(data)

}


