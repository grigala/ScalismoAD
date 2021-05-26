package ch.grigala.scalismoad.sampling

import ch.grigala.scalismoad.data.Sample
import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.rule.ScalarRule.Implicits._

case class NormalGaussianLogLikelihood(theta: Sample, data: Seq[(Double, Double)]) {

    private val a: Var[Scalar, Double] = Var(theta.parameters.a)
    private val b: Var[Scalar, Double] = Var(theta.parameters.b)
    private val sig: Var[Scalar, Double] = Var(theta.parameters.sigma2)
    private val logNormalizer: Node[Scalar, Double] = log(sqrt(2.0 * scala.math.Pi)) + log(sig)
    private var computational_graph: Node[Scalar, Double] = Var(0.0)

    private lazy val calculateGradient = computational_graph.grad()

    for ((x, y) <- data) yield {
        val f = -0.5 * pow(y - (a * x + b) / sig, 2) - logNormalizer
        println(f)
        computational_graph = computational_graph + f
    }

    def computationalGraph: Node[Scalar, Double] = {
        computational_graph
    }

    def value: Double = {
        val scalarValue = computational_graph.apply().unwrapNonContainerValue
        scalarValue
    }

    def gradients: (Double, Double, Double) = {
        calculateGradient
        val dfda = a.gradient.unwrapNonContainerValue
        val dfdb = b.gradient.unwrapNonContainerValue
        val dfdsig = sig.gradient.unwrapNonContainerValue
        (dfda, dfdb, dfdsig)
    }

    def fullGradient: Double = {
        calculateGradient.unwrapNonContainerValue
    }

}
