package ch.grigala.scalismoad.value

import ch.grigala.scalismoad.graph.Scalar
import ch.grigala.scalismoad.rule.ValueRule

case class ContainerValue[U[_], T](data: U[T]) extends Value[U, T] {

    type C = ContainerValue[U, T]
    type NC = NonContainerValue[U, T]

    def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.addCC(data, r.data))
            case r: NC => ContainerValue[U, T](vr.addCM(data, r.data))
        }
    }

    def +(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.addCC(data, rhs))
    }

    def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.subCC(data, r.data))
            case r: NC => ContainerValue[U, T](vr.subCM(data, r.data))
        }
    }

    def -(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.subCC(data, rhs))
    }

    def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.mulCC(data, r.data))
            case r: NC => ContainerValue[U, T](vr.mulCM(data, r.data))
        }
    }

    def *(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.mulCC(data, rhs))
    }

    def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.divCC(data, r.data))
            case r: NC => ContainerValue[U, T](vr.divCM(data, r.data))
        }
    }

    def /(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.divCC(data, rhs))
    }

    def <(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.ltCC(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.ltCM(data, r.data))
        }
    }

    def <(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.ltCC(data, rhs))
    }

    def <=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.lteCC(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.lteCM(data, r.data))
        }
    }

    def <=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.lteCC(data, rhs))
    }

    def >(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.gtCC(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.gtCM(data, r.data))
        }
    }

    def >(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.gtCC(data, rhs))
    }

    def >=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.gteCC(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.gteCM(data, r.data))
        }
    }

    def >=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.gteCC(data, rhs))
    }

    def ==(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.eqCC(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.eqCM(data, r.data))
        }
    }

    def ==(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.eqCC(data, rhs))
    }

    def close(rhs: Value[U, T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.closeCC(data, r.data, eps))
            case r: NC => ContainerValue[U, Boolean](vr.closeCM(data, r.data, eps))
        }
    }

    def close(rhs: U[T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.closeCC(data, rhs, eps))
    }

    def unary_+(implicit vr: ValueRule[U, T]): Value[U, T] = ContainerValue[U, T](vr.posC(data))

    def unary_-(implicit vr: ValueRule[U, T]): Value[U, T] = ContainerValue[U, T](vr.negC(data))

    def T()(implicit vr: ValueRule[U, T]): Value[U, T] = ContainerValue[U, T](vr.transposeC(data))

    override def unwrap: Scalar[_] = data.asInstanceOf[Scalar[_]]
}
