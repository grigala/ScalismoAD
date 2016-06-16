package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph.bool.{BroadcastLeft2C, BroadcastRight2C, Elementwise2C, InferElementwise2C}
import com.kogecoo.scalaad.op.bool.{Eq, Gt, Gte, Lt, Lte, Neq}
import com.kogecoo.scalaad.op.{Add, Div, Dot, Mul, Neg, Pos, Sub}
import shapeless.{Nat, Succ}
import shapeless.ops.nat.Sum



trait ValueExpr[N <: Nat]  extends Expr[N]{

  final def forward[W <: Nat, O <: Nat](wrt: ValueExpr[W])(implicit s: Sum.Aux[N, W, O]): ValueExpr[O] = {
    _forward[W, s.Out](wrt)
  }

  final def reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = _reverse(g)

  def eval[R](implicit E: Eval[ValueExpr[N], R]): R = E.eval(this)

  def _forward[W <: Nat, O <: Nat](wrt: ValueExpr[W]): ValueExpr[O]

  def _reverse[G <: Nat](adj: ValueExpr[G]): Grad[G]

}


object ValueExpr {

  implicit class RichValueExprFold[N <: Nat](val self: V[Succ[N]]) extends AnyVal {

    def dot(rhs: V[Succ[N]]): V[N] = Fold2[N](self, rhs, Dot, 0)

  }

  implicit class RichValueExpr[N <: Nat](val self: V[N]) extends AnyVal {

    def unary_+(): V[N] = Elementwise1[N](self, Pos)
    def unary_-(): V[N] = Elementwise1[N](self, Neg)

    def +(rhs: V[N]): V[N] = Elementwise2[N](self, rhs, Add)
    def -(rhs: V[N]): V[N] = Elementwise2[N](self, rhs, Sub)
    def *(rhs: V[N]): V[N] = Elementwise2[N](self, rhs, Mul)
    def /(rhs: V[N]): V[N] = Elementwise2[N](self, rhs, Div)

    def +:>[M <: Nat](rhs: V[M]): V[N] = BroadcastLeft2[N, M](self, rhs, Add)
    def -:>[M <: Nat](rhs: V[M]): V[N] = BroadcastLeft2[N, M](self, rhs, Sub)
    def *:>[M <: Nat](rhs: V[M]): V[N] = BroadcastLeft2[N, M](self, rhs, Mul)
    def /:>[M <: Nat](rhs: V[M]): V[N] = BroadcastLeft2[N, M](self, rhs, Div)

    def +:<[M <: Nat](rhs: V[M]): V[M] = BroadcastRight2[N, M](self, rhs, Add)
    def -:<[M <: Nat](rhs: V[M]): V[M] = BroadcastRight2[N, M](self, rhs, Sub)
    def *:<[M <: Nat](rhs: V[M]): V[M] = BroadcastRight2[N, M](self, rhs, Mul)
    def /:<[M <: Nat](rhs: V[M]): V[M] = BroadcastRight2[N, M](self, rhs, Div)

    def ==(rhs: V[N]): B[N] = Elementwise2C(self, rhs, Eq)
    def !=(rhs: V[N]): B[N] = Elementwise2C(self, rhs, Neq)
    def < (rhs: V[N]): B[N] = Elementwise2C(self, rhs, Lt)
    def <=(rhs: V[N]): B[N] = Elementwise2C(self, rhs, Lte)
    def > (rhs: V[N]): B[N] = Elementwise2C(self, rhs, Gt)
    def >=(rhs: V[N]): B[N] = Elementwise2C(self, rhs, Gte)

    def ==:<[M <: Nat](rhs: V[M]): B[N] = BroadcastLeft2C(self, rhs, Eq)
    def !=:<[M <: Nat](rhs: V[M]): B[N] = BroadcastLeft2C(self, rhs, Neq)
    def <:< [M <: Nat](rhs: V[M]): B[N] = BroadcastLeft2C(self, rhs, Lt)
    def <=:<[M <: Nat](rhs: V[M]): B[N] = BroadcastLeft2C(self, rhs, Lte)
    def >:< [M <: Nat](rhs: V[M]): B[N] = BroadcastLeft2C(self, rhs, Gt)
    def >=:<[M <: Nat](rhs: V[M]): B[N] = BroadcastLeft2C(self, rhs, Gte)

    def ==:>[M <: Nat](rhs: V[M]): B[M] = BroadcastRight2C(self, rhs, Eq)
    def !=:>[M <: Nat](rhs: V[M]): B[M] = BroadcastRight2C(self, rhs, Neq)
    def <:> [M <: Nat](rhs: V[M]): B[M] = BroadcastRight2C(self, rhs, Lt)
    def <=:>[M <: Nat](rhs: V[M]): B[M] = BroadcastRight2C(self, rhs, Lte)
    def >:> [M <: Nat](rhs: V[M]): B[M] = BroadcastRight2C(self, rhs, Gt)
    def >=:>[M <: Nat](rhs: V[M]): B[M] = BroadcastRight2C(self, rhs, Gte)

    // type unsafe operations
    def :+[M <: Nat, O <: Nat](rhs: V[M]): V[O] = InferElementwise2[O, N, M](self, rhs, Add)
    def :-[M <: Nat, O <: Nat](rhs: V[M]): V[O] = InferElementwise2[O, N, M](self, rhs, Sub)
    def :*[M <: Nat, O <: Nat](rhs: V[M]): V[O] = InferElementwise2[O, N, M](self, rhs, Mul)
    def :/[M <: Nat, O <: Nat](rhs: V[M]): V[O] = InferElementwise2[O, N, M](self, rhs, Div)

    def :==[M <: Nat, O <: Nat](rhs: V[M]): B[O] = InferElementwise2C(self, rhs, Eq)
    def :!=[M <: Nat, O <: Nat](rhs: V[M]): B[O] = InferElementwise2C(self, rhs, Neq)
    def :< [M <: Nat, O <: Nat](rhs: V[M]): B[O] = InferElementwise2C(self, rhs, Lt)
    def :<=[M <: Nat, O <: Nat](rhs: V[M]): B[O] = InferElementwise2C(self, rhs, Lte)
    def :> [M <: Nat, O <: Nat](rhs: V[M]): B[O] = InferElementwise2C(self, rhs, Gt)
    def :>=[M <: Nat, O <: Nat](rhs: V[M]): B[O] = InferElementwise2C(self, rhs, Gte)
  }

}

