package ch.grigala.ad.example

import breeze.linalg.DenseVector
import ch.grigala.ad.graph.{Var, sin}

object Application {
    def main(args: Array[String]) = {
        scalismo.initialize()
        implicit val rng = scalismo.utils.Random(42)

        val a = 0.2
        val b = 3
        val sigma2 = 0.5
        val errorDist = breeze.stats.distributions.Gaussian(0, sigma2)
        val data = for (x <- 0 until 100) yield {
            (x.toDouble, a * x + b + errorDist.draw())
        }
        scalarExample()
        breezeExample()
    }

    def scalarExample() = {
        import ch.grigala.ad.rule.ScalarRule.Implicits._

        val x = Var(5.0)
        val y = Var(3.0)
        val z = x * sin(x) * 2 + y * x * 3

        println(z)
        println(z.deriv(x)) // forward-mode automatic differentiation
        println(z.deriv(y))

        println(z.grad()) // reverse-mode automatic differentiation
        println(x.gradient) // we can get partial differentiation through gradient after run grad()
        println(y.gradient)
    }

    def breezeExample() = {
        import ch.grigala.ad.rule.BreezeRule.Implicits._

        val x = Var(DenseVector(1.0, 2.0, 3.0))

        val y = 1 * sin(x) * 2 + x * 3

        println(y)
        println(y.deriv(x))

        println(y.grad())
        println(x.gradient)
    }

}
