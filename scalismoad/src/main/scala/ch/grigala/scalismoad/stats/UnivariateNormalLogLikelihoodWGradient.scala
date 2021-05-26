package ch.grigala.scalismoad.stats

import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.rule.ScalarRule.Implicits._

/**
 * Calculates a normalized log density of 1D Gaussian with given mean and variance
 * As a bonus it calculates gradients of the mean and variance
 *
 * @param mu    mean of the gaussian
 * @param sigma variance of the gaussian
 * @param data  point(s) at which pdf is evaluated
 */
case class UnivariateNormalLogLikelihoodWGradient(mu: Double, sigma: Double, data: Seq[Double]) {
    private val mean = Var(mu)
    private val variance = Var(sigma)
    private val logNormalizer = log(sqrt(2.0 * scala.math.Pi)) + log(variance)
    private var compGraph: Node[Scalar, Double] = Var(0.0)


    for (x <- data) yield {
        val f = Neg(0.5 * pow(x - mean, 2) / (variance * variance)) - logNormalizer
        compGraph = compGraph + f
    }

    def computationalGraph: Node[Scalar, Double] = {
        compGraph
    }

    def value: Double = {
        val scalarValue = compGraph.apply().unwrapContainerValue.data
        scalarValue
    }

    def gradients: (Double, Double) = {
        // Calculates full gradient and stores partial differential values in .gradient
        val grad = compGraph.grad()

        val dzdmu = mean.gradient.unwrapContainerValue.data
        val dzdsigma = variance.gradient.unwrapContainerValue.data
        (dzdmu, dzdsigma)
    }
}
