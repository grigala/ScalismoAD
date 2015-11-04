package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.graph.bool._
import com.kogecoo.scalaad.impl.std.{StdElementwiseOp, StdElementwiseOpB, StdElementwiseOpC}


// copy from StdScalarEval
trait Nd4jScalarEval { self: Nd4jTensorEval with Nd4jValue =>

  implicit val eval_nd4j_scalar_double: Eval[V, T0] = new Eval[V, T0] {

    def eval(n: V): T0 = n.shape.order match {
      case 0 => n match {

        // Nullary op
        case Var(d)    => d.value[T0]
        case Const(d)  => d.value[T0]
        case Diag(d, _) => d.value[T0]
        case _: Eye     => 1.0

        // Unary op
        case Sum1(v, axis) => v.eval[T].sumNumber().doubleValue()
        case Max1(v, axis) => v.eval[T].maxNumber().doubleValue()
        case Min1(v, axis) => v.eval[T].minNumber().doubleValue()

        // Binary op
        case Dot(l, r) => Nd4jUtil.dot(l.eval[T], r.eval[T])

        // Ternary op
        case ElementwiseWhere(cond, a, b) => {
          if (cond.eval[B0]) a.eval[T0] else b.eval[T0]
        }

        // Elementwise
        case e: Apply0 =>
          val f = StdElementwiseOp.nullary(e)
          f()

        case e: Elementwise1 =>
          val f = StdElementwiseOp.unary(e)
          f(e.v.eval[T0])

        case e: Elementwise2 =>
          val f = StdElementwiseOp.binary(e)
          f(e.l.eval[T0], e.r.eval[T0])

      }
    }
  }

  implicit val eval_nd4j_scalar_bool: Eval[B, B0] = new Eval[B, B0] {

    def eval(n: B): B0 = n.shape.order match {
      case 0 => n match {
        case e: Elementwise1B =>
          val f = StdElementwiseOpB.unary(e)
          f(e.v.eval[B0])

        case e: Elementwise2B =>
          val f = StdElementwiseOpB.binary(e)
          f(e.l.eval[B0], e.r.eval[B0])

        case e: Elementwise2C =>
          val f = StdElementwiseOpC.binary(e)
          f(e.l.eval[T0], e.r.eval[T0])
      }
    }
  }

}
