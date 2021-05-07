package ch.grigala.scalismoad.sampling

import ch.grigala.scalismoad.data.Sample
import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.rule.ScalarRule.Implicits._

case class NormalGaussianLogLikelihood(theta: Sample, data: Seq[(Double, Double)]) {

    private val a: Var[Scalar, Double] = Var(theta.parameters.a)
    private val b: Var[Scalar, Double] = Var(theta.parameters.b)
    private val sig: Var[Scalar, Double] = Var(theta.parameters.sigma2)
    private val logNormalizer: Node[Scalar, Double] = 0.5 * log(sqrt(2.0 * scala.math.Pi)) + log(sig)
    private var computational_graph: Node[Scalar, Double] = Var(0.0)

    private lazy val calculateGradient = computational_graph.grad()

    for ((x, y) <- data) yield {
        val f = 0.5 * Neg(pow(y - (a * x + b), 2) / sig) - logNormalizer
        computational_graph = computational_graph + f
    }

    def computationalGraph: Node[Scalar, Double] = {
        computational_graph
    }

    def value: Double = {
        val scalarValue = computational_graph.apply().unwrap
        scalarValue.data.asInstanceOf[Double]
    }

    def gradients: (Double, Double, Double) = {
        calculateGradient
        val dfda = a.gradient.unwrap.data.asInstanceOf[Double]
        val dfdb = b.gradient.unwrap.data.asInstanceOf[Double]
        val dfdsig = sig.gradient.unwrap.data.asInstanceOf[Double]
        (dfda, dfdb, dfdsig)
    }

    def fullGradient: Double = {
        calculateGradient.unwrap.data.asInstanceOf[Double]
    }

}
