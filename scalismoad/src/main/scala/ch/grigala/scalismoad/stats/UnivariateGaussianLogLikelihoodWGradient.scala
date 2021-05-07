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
case class UnivariateGaussianLogLikelihoodWGradient(mu: Double, sigma: Double, data: Seq[Double]) {
    private val mean = Var(mu)
    private val variance = Var(sigma)
    private val logNormalizer = log(sqrt(2.0 * scala.math.Pi)) + log(sigma)
    private var compGraph: Node[Scalar, Double] = Var(0.0)

    for (x <- data) yield {
        val f = Neg(0.5 * pow(x - mu, 2) / (sigma * sigma)) - logNormalizer
        compGraph = compGraph + f
    }

    def computationalGraph: Node[Scalar, Double] = {
        compGraph
    }

    def value: Double = {
        val scalarValue = compGraph.apply().unwrap
        scalarValue.data.asInstanceOf[Double]
    }

    def gradients: (Double, Double) = {
        // Calculates full gradient and stores partial differential values in .gradient
        val ∇ = compGraph.grad()

        val `∂z∕∂mu` = mean.gradient.unwrap.data.asInstanceOf[Double]
        val `∂z∕∂sigma` = variance.gradient.unwrap.data.asInstanceOf[Double]
        (`∂z∕∂mu`, `∂z∕∂sigma`)
    }
}
