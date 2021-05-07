package ch.grigala.scalismoad.example

import breeze.linalg.DenseVector
import ch.grigala.scalismoad.graph.{Neg, Var, sin}

object Application {
    def main(args: Array[String]): Unit = {

        scalismo.initialize()

        scalarExample()
//        breezeExample()
    }

    def scalarExample(): Unit = {
        import ch.grigala.scalismoad.rule.ScalarRule.Implicits._

        val x = Var(5.0)
        val y = Var(2.0)
        //        val z = y * sin(x) + pow(y, 2)
        val z = x * (Neg(sin(x * y)) + y) * 4

        println(s"Value=${z.apply()}")
        println(s"∂z∕∂x=${z.deriv(x)}")
        println(s"∂z∕∂y=${z.deriv(y)}")
        z.grad()

        println(s"∇z={${x.gradient}, ${y.gradient}}")

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
