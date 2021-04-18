package ch.grigala.scalismoad.example

import breeze.linalg.DenseVector
import ch.grigala.scalismoad.graph.{Neg, Scalar, ScalarConst, Var, asin, cos, ln, pow, sin}

object Application {
    def main(args: Array[String]): Unit = {

        scalismo.initialize()

        scalarExample()
//        breezeExample()
    }

    def scalarExample(): Unit = {
        import ch.grigala.scalismoad.rule.ScalarRule.Implicits._

//        val x = Var(5.0)
//        val y = Var(3.0)
//        val z = x * sin(x) * 2 + y * x * 3
//
//        println(z)
//        println(doubleWrapperRule.toWrappee(z.deriv(x).unwrap.asInstanceOf[Scalar[_]])) // forward-mode automatic differentiation
//        println(doubleWrapperRule.toWrappee(z.deriv(y).unwrap.asInstanceOf[Scalar[_]]))
//
//        println(doubleWrapperRule.toWrappee(z.grad().unwrap.asInstanceOf[Scalar[_]])) // reverse-mode automatic differentiation
//        println(doubleWrapperRule.toWrappee(x.gradient.unwrap.asInstanceOf[Scalar[_]])) // we can get partial differentiation through gradient after run grad()
//        println(doubleWrapperRule.toWrappee(y.gradient.unwrap.asInstanceOf[Scalar[_]]))

//        import ch.grigala.scalismoad.rule.BreezeRule.Implicits._

//        val x1 = Var(3.0)
//        val x2 = Var(5.0)
//
//        val f = ln(x1) + x1 * x2 - sin(x2)
//
////        val f = pow(x1,  2) + pow(x2, 3)
//        println(f.deriv(x1))
//        println(f.deriv(x2))
//
//        println(f.grad())

        val x = Var(5.0)
        val y = Var(2.0)
//        val z = y * sin(x) + pow(y, 2)
        val z = x * (Neg(sin(x * y)) + y) * 4

        // forward-mode automatic differentiation
        // partial differentiation w.r.t x
        println(z)
        println("∂z∕∂x=" + z.deriv(x).unwrap.data)
        println("∂z∕∂y=" +z.deriv(y).unwrap.data)

        // reverse-mode automatic differentiation computes a gradient
        println(z.grad())

        // we can get partial differentiation through `gradient` after running grad()
        println(x.gradient)
        println(y.gradient)

    }

    def breezeExample(): Unit = {
        import ch.grigala.scalismoad.rule.BreezeRule.Implicits._

        val x = Var(DenseVector(1.0, 2.0, 3.0))

        val y = 1 * sin(x) * 2 + x * 3

        println(y)
        println(y.deriv(x).unwrap.asInstanceOf[DenseVector[Double]])

        println(y.grad().unwrap.asInstanceOf[DenseVector[Double]])
        println(x.gradient.unwrap.asInstanceOf[DenseVector[Double]])
    }

}
