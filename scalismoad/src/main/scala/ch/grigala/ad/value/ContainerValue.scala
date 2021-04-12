package ch.grigala.ad.value

import ch.grigala.ad.rule.ValueRule

case class ContainerValue[U[_], T](data: U[T]) extends Value[U, T] {

    type C = ContainerValue[U, T]
    type NC = NonContainerValue[U, T]

    def +(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.addSS(data, r.data))
            case r: NC => ContainerValue[U, T](vr.addSM(data, r.data))
        }
    }

    def +(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.addSS(data, rhs))
    }

    def -(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.subSS(data, r.data))
            case r: NC => ContainerValue[U, T](vr.subSM(data, r.data))
        }
    }

    def -(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.subSS(data, rhs))
    }

    def *(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.mulSS(data, r.data))
            case r: NC => ContainerValue[U, T](vr.mulSM(data, r.data))
        }
    }

    def *(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.mulSS(data, rhs))
    }

    def /(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        rhs match {
            case r: C => ContainerValue[U, T](vr.divSS(data, r.data))
            case r: NC => ContainerValue[U, T](vr.divSM(data, r.data))
        }
    }

    def /(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, T] = {
        ContainerValue[U, T](vr.divSS(data, rhs))
    }

    def <(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.ltSS(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.ltSM(data, r.data))
        }
    }

    def <(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.ltSS(data, rhs))
    }

    def <=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.lteSS(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.lteSM(data, r.data))
        }
    }

    def <=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.lteSS(data, rhs))
    }

    def >(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.gtSS(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.gtSM(data, r.data))
        }
    }

    def >(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.gtSS(data, rhs))
    }

    def >=(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.gteSS(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.gteSM(data, r.data))
        }
    }

    def >=(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.gteSS(data, rhs))
    }

    def ==(rhs: Value[U, T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.eqSS(data, r.data))
            case r: NC => ContainerValue[U, Boolean](vr.eqSM(data, r.data))
        }
    }

    def ==(rhs: U[T])(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.eqSS(data, rhs))
    }

    def close(rhs: Value[U, T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        rhs match {
            case r: C => ContainerValue[U, Boolean](vr.closeSS(data, r.data, eps))
            case r: NC => ContainerValue[U, Boolean](vr.closeSM(data, r.data, eps))
        }
    }

    def close(rhs: U[T], eps: T)(implicit vr: ValueRule[U, T]): Value[U, Boolean] = {
        ContainerValue[U, Boolean](vr.closeSS(data, rhs, eps))
    }

    def unary_+(implicit vr: ValueRule[U, T]): Value[U, T] = ContainerValue[U, T](vr.posS(data))

    def unary_-(implicit vr: ValueRule[U, T]): Value[U, T] = ContainerValue[U, T](vr.negS(data))

    def T()(implicit vr: ValueRule[U, T]): Value[U, T] = ContainerValue[U, T](vr.transposeS(data))

    override def unwrap: U[T] = data
}
