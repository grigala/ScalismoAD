package com.kogecoo.scalaad

import shapeless.Nat.{_0, _1, _2}
import shapeless.{Nat, Sized}

/**
  * containers used for carrying
  * tensor shape (dimensions for each axis) parameter(s).
  */
class Shape[N <: Nat](val underlying: Sized[List[Int], N]) {

  def apply(i: Int): Int = underlying.unsized(i)

  //def extend[M <: Nat, O <: Nat](other: Shape[M])(implicit sum: Sum.Aux[N, M, O]): Shape[O] = {
  def extend[M <: Nat, O <: Nat](other: Shape[M]): Shape[O] = {
    val extend = underlying.unsized ++ other.underlying.unsized
    new Shape[O](Sized.wrap[List[Int], O](extend))
  }

  def order: Int = underlying.unsized.length
}


object Shape0 {

  def apply(): Shape[_0] = new Shape[_0](Sized[List]())

}


// default shape is a column vector
object Shape1 {

  def apply(l: Int): Shape[_1] = new Shape[_1](Sized[List](l))

}


object Shape2 {

  def apply(r: Int, c: Int): Shape[_2] = new Shape[_2](Sized[List](r, c))

}

