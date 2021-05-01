package ch.grigala.scalismoad.sampling

import breeze.numerics.constants.Pi
import ch.grigala.scalismoad.data.Sample
import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.rule.ScalarRule.Implicits._

case class GaussianCompGraph(theta: Sample, data: Seq[(Double, Double)]) {

    private val a: Var[Scalar, Double] = Var(theta.parameters.a)
    private val b: Var[Scalar, Double] = Var(theta.parameters.b)
    private val sig: Var[Scalar, Double] = Var(theta.parameters.sigma2)
    private val pi: Var[Scalar, Double] = Var(Pi)
    private val den: Var[Scalar, Double] = Var(2.0)
    private val logNormalizer: Node[Scalar, Double] = ln(sqrt(2.0 * pi)) + ln(sig)
    private var computational_graph: Node[Scalar, Double] = Var(0.0)

    private lazy val calculateGradient = computational_graph.grad()

    for ((x, y) <- data) yield {
        val f = Neg(pow(y - (a * x + b), 2) / sig) / den - logNormalizer
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
        val `∂f∕∂a` = a.gradient.unwrap.data.asInstanceOf[Double]
        val `∂f∕∂b` = b.gradient.unwrap.data.asInstanceOf[Double]
        val `∂f∕∂sig` = sig.gradient.unwrap.data.asInstanceOf[Double]
        (`∂f∕∂a`, `∂f∕∂b`, `∂f∕∂sig`)
    }

    def fullGradient: Double = {
        calculateGradient.unwrap.data.asInstanceOf[Double]
    }

}
