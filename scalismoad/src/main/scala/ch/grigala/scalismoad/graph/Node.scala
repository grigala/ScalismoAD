package ch.grigala.scalismoad.graph

import ch.grigala.scalismoad.rule.ValueRule
import ch.grigala.scalismoad.value.Value

// The most fundamental component of computational graph for automatic derivation.
// Every node in a computational graph must inherit Node.
//
//  Node doesn't know actual computational rule (how to calculate +, -, *, ...),
// so if you want to compute derivation on your own class (ComplexNumber, Matrix, etc),
// you need to define its computational rules (see the definition of ValueRule).

trait Node[U[_], T] {
    def toString: String

    def apply(): Value[U, T]

    def deriv(wrt: Var[U, T]): Value[U, T] // compute with forward-mode automatic differentiation

    def propagate(g: Value[U, T]): Value[U, T] // compute with reverse-mode autmatic differentiation

    def grad()(implicit r: ValueRule[U, T]): Value[U, T] = {
        propagate(r.one)
    }

    def +(rhs: Node[U, T])(implicit r: ValueRule[U, T]): Node[U, T] = Add(this, rhs)

    def -(rhs: Node[U, T])(implicit r: ValueRule[U, T]): Node[U, T] = Sub(this, rhs)

    def *(rhs: Node[U, T])(implicit r: ValueRule[U, T]): Node[U, T] = Mul(this, rhs)

    def /(rhs: Node[U, T])(implicit r: ValueRule[U, T]): Node[U, T] = Div(this, rhs)

    def unary_+(rhs: Node[U, T])(implicit r: ValueRule[U, T]): Node[U, T] = Pos(this)

    def unary_-(rhs: Node[U, T])(implicit r: ValueRule[U, T]): Node[U, T] = Neg(this)

    def T()(implicit r: ValueRule[U, T]): Node[U, T] = Transpose(this)

}
