package ch.grigala.scalismoad.example

import breeze.linalg.DenseVector
import ch.grigala.scalismoad.sampling.SampleLens
import ch.grigala.scalismoad.sampling.evaluators.ProductEvaluator
import ch.grigala.scalismoad.sampling.proposals.MalaProposal
import scalismo.sampling.algorithms.MetropolisHastings
import scalismo.sampling.{DistributionEvaluator, GradientEvaluator}

// Parameter class - This is generic and every application chooses its
// own structures of the parameters
case class MALAParameters(mu: Double, sigma: Double)

case class MALASample(parameters: MALAParameters, generatedBy: String)


// This typeclass instance is used to provide access to allow the
// generically implemented proposals access the parameters
object AllParamLens extends SampleLens[MALASample, DenseVector[Double]] {
    override def get(sample: MALASample): DenseVector[Double] = {
        val v = DenseVector.zeros[Double](2)
        v(0) = sample.parameters.mu
        v(1) = sample.parameters.sigma
        v
    }

    override def replace(
                            sample: MALASample,
                            vector: DenseVector[Double],
                            generatedBy: Option[String]
                        ): MALASample = {
        sample.copy(parameters =
            sample.parameters.copy(mu = vector(0), sigma = vector(1))
        )
    }
}

// Gaussian likelihood - with gradient
case class LikelihoodEvaluator(data: Seq[Double])
    extends DistributionEvaluator[MALASample]
        with GradientEvaluator[MALASample] {

    override def logValue(theta: MALASample): Double = {
        val likelihood = breeze.stats.distributions.Gaussian(
            theta.parameters.mu,
            theta.parameters.sigma
        )
        val likelihoods = for (x <- data) yield {
            likelihood.logPdf(x)
        }
        likelihoods.sum
    }

    override def gradient(theta: MALASample): MALASample = {
        val mu = theta.parameters.mu
        val sigma = theta.parameters.sigma
        val sigma2 = sigma * sigma
        val sigma3 = sigma2 * sigma
        val n = data.length

        val dmu = 1.0 / sigma2 * data.map(x => x - mu).sum
        val dsigma = -n / sigma + 1.0 / sigma3 * data.map(x => (x - mu) * (x - mu)).sum

        val params = theta.parameters.copy(mu = dmu, sigma = dsigma)
        theta.copy(parameters = params)
    }

}

// Gaussian priors on the parameters. Also the prior needs to have a gradient
object PriorEvaluator
    extends DistributionEvaluator[MALASample]
        with GradientEvaluator[MALASample] {

    val priorDistMu = breeze.stats.distributions.Gaussian(0, 10)
    val priorDistSigma = breeze.stats.distributions.Gaussian(0, 20)

    override def logValue(theta: MALASample): Double = {
        priorDistMu.logPdf(theta.parameters.mu) + priorDistSigma.logPdf(
            theta.parameters.sigma
        )
    }

    override def gradient(sample: MALASample): MALASample = {
        val gradMu = -(sample.parameters.mu - priorDistMu.mean) / priorDistMu.variance
        val gradSigma = -(sample.parameters.sigma - priorDistSigma.mean) / priorDistSigma.variance
        sample.copy(parameters = sample.parameters.copy(mu = gradMu, sigma = gradSigma))
    }
}

object MALAExample {

    def generateSyntheticData(numDataPoints: Int): Seq[Double] = {
        // The true distribution to sample from
        val mu = -3
        val sigma = 12

        val trueDistribution = breeze.stats.distributions.Gaussian(mu, sigma)
        for (_ <- 0 until numDataPoints) yield {
            trueDistribution.draw()
        }
    }

    def main(args: Array[String]): Unit = {

        scalismo.initialize()
        implicit val rng = scalismo.utils.Random(42)

        val data = generateSyntheticData(100)

        // We use the new method  withGradient of the productEvaluator to combine evaluators that have gradients
        val posteriorEvaluator = ProductEvaluator.withGradient[MALASample](PriorEvaluator, LikelihoodEvaluator(data))

        // Using the new Mala proposal as a gradient
        val generator = MalaProposal(posteriorEvaluator, 1e-3, AllParamLens)

        // From here on, the rest ist standard MH sampling

        val chain = MetropolisHastings(generator, posteriorEvaluator)

        val initialSample = MALASample(MALAParameters(0, 1), generatedBy = "initial")
        val mhIterator = chain.iterator(initialSample)
        val samples = mhIterator.drop(1000).take(10000).toIndexedSeq

        val estimatedMean =
            samples.map(sample => sample.parameters.mu).sum / samples.size
        val estimatedSigma =
            samples.map(sample => sample.parameters.sigma).sum / samples.size

        println((estimatedMean, estimatedSigma))
    }
}
