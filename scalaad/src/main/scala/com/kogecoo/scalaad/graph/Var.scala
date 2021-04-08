package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.{ValueRule, ValueWrapperRule}
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue, Value}


class Var[U[_], T](val data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T] {
    var gradient: Value[U, T] = r.zero(data)

    override def toString: String = s"Var[${data}]"

    override def apply(): Value[U, T] = r.toValue(data)

    override def deriv(wrt: Var[U, T]): Value[U, T] = wrt() match {
        case v: NonContainerValue[U, T] => if (wrt == this) r.one(data) else r.zero(data)
        case v: ContainerValue[U, T] => if (wrt == this) r.one(v) else r.zero(v)
    }

    override def propagate(g: Value[U, T]): Value[U, T] = {
        val v = g * r.one(data)
        gradient += v
        v
    }
}

object Var {
    def apply[U[_], T](data: U[T])(implicit r: ValueRule[U, T]): Var[U, T] = new Var[U, T](data)

    def apply[Src, U[_], T](data: Src)(implicit r: ValueRule[U, T], f: ValueWrapperRule[Src, U, T]): Var[U, T] = {
        new Var[U, T](f.toWrapper(data))
    }
}
