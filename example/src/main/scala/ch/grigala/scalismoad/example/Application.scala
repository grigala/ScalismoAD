package ch.grigala.scalismoad.example

import breeze.linalg.{DenseMatrix, DenseVector}
import ch.grigala.scalismoad.graph.{Mul, Neg, Node, Scalar, Var, dot, log, sin}
import ch.grigala.scalismoad.rule.DoubleRule.Implicits.{doubleRule, doubleWrapperRule}

object Application {
    def main(args: Array[String]): Unit = {

        scalismo.initialize()
        breeze.linalg.operators.OpMulMatrix
        //        scalarExample()
        breezeExample()
    }

    def scalarExample(): Unit = {
        import ch.grigala.scalismoad.rule.ScalarRule.Implicits._

        val x: Var[Scalar, Double] = Var(2.0)
        val y = Var(5.0)
        //        val z = y * sin(x) + pow(y, 2)
        val z: Node[Scalar, Double] = log(x) + x * y - sin(y)
        println(z)
        println(s"Value=${z.apply().unwrapContainerValue.data}")
        println(s"∂z∕∂x=${z.deriv(x).unwrapContainerValue.data}")
        println(s"∂z∕∂y=${z.deriv(y).unwrapContainerValue.data}")
        println(z.grad().unwrapContainerValue.data)
        println(s"∇z={${x.gradient.unwrapContainerValue.data}, ${y.gradient.unwrapContainerValue.data}}")
    }

    def breezeExample(): Unit = {
        import ch.grigala.scalismoad.rule.BreezeRule.Implicits._

        val m1 = DenseMatrix.rand[Double](3, 3)
        val m2 = DenseMatrix.rand[Double](3, 3)
        val v1 = DenseVector.rand[Double](3)
        val v2 = DenseVector.rand[Double](3)

        //        val dotMatrix = m1.t * m2
        //        val dotVector = v1 dot v2

        val mm1 = Var(m1)
        val mm2 = Var(m2)

        val vv1 = Var(v1)
        val vv2 = Var(v2)

        println((mm1 * mm2).apply().unwrapContainerValue)
        println(dot(vv1, vv2).apply().unwrapContainerValue)

    }

}
