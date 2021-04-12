package ch.grigala.scalismoad.example

import breeze.linalg.DenseVector
import ch.grigala.scalismoad.graph.{Scalar, Var, sin}

object Application {
    def main(args: Array[String]): Unit = {

        scalismo.initialize()

        scalarExample()
        breezeExample()
    }

    def scalarExample(): Unit = {
        import ch.grigala.scalismoad.rule.ScalarRule.Implicits._

        val x = Var(5.0)
        val y = Var(3.0)
        val z = x * sin(x) * 2 + y * x * 3

        println(z)
        println(doubleWrapperRule.toWrappee(z.deriv(x).unwrap.asInstanceOf[Scalar[_]])) // forward-mode automatic differentiation
        println(doubleWrapperRule.toWrappee(z.deriv(y).unwrap.asInstanceOf[Scalar[_]]))

        println(doubleWrapperRule.toWrappee(z.grad().unwrap.asInstanceOf[Scalar[_]])) // reverse-mode automatic differentiation
        println(doubleWrapperRule.toWrappee(x.gradient.unwrap.asInstanceOf[Scalar[_]])) // we can get partial differentiation through gradient after run grad()
        println(doubleWrapperRule.toWrappee(y.gradient.unwrap.asInstanceOf[Scalar[_]]))
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
