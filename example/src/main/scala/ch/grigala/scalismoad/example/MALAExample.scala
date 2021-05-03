package ch.grigala.scalismoad.example

import breeze.linalg.DenseVector
import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.logging.VerbosePrintLogger
import ch.grigala.scalismoad.rule.ScalarRule.Implicits._
import ch.grigala.scalismoad.sampling.SampleLens
import ch.grigala.scalismoad.sampling.evaluators.ProductEvaluator
import ch.grigala.scalismoad.sampling.proposals.MalaProposal
import scalismo.sampling.algorithms.MetropolisHastings
import scalismo.sampling.loggers.AcceptRejectLogger
import scalismo.sampling.{DistributionEvaluator, GradientEvaluator, ProposalGenerator}
import spire.algebra.Semigroup

// Parameter class - This is generic and every application chooses its
// own structures of the parameters
case class MALAParameters(mu: Double, sigma: Double)

case class MALASample(parameters: MALAParameters, generatedBy: String)

object MALASample {
    implicit object semiGroupMalaSample extends Semigroup[MALASample] {
        override def combine(x: MALASample, y: MALASample): MALASample = {
            x.copy(parameters = MALAParameters(
                mu = x.parameters.mu + y.parameters.mu,
                sigma = x.parameters.sigma + y.parameters.sigma
            ))
        }
    }
}

// This typeclass instance is used to provide access to allow the
// generically implemented proposals access the parameters
object AllParamLens extends SampleLens[MALASample, DenseVector[Double]] {
    override def get(sample: MALASample): DenseVector[Double] = {
        val v = DenseVector.zeros[Double](2)
        v(0) = sample.parameters.mu
        v(1) = sample.parameters.sigma
        v
    }

    override def replace(fullSample: MALASample, partial: DenseVector[Double], generatedBy: Option[String]): MALASample = {
        val params = fullSample.parameters.copy(mu = partial(0), sigma = partial(1))
        fullSample.copy(parameters = params)
    }
}

// Gaussian likelihood - with gradient
case class MALALikelihoodEvaluator(data: Seq[Double])
    extends DistributionEvaluator[MALASample]
        with GradientEvaluator[MALASample] {

    //    f(x, mu, sigma) = log(Normal(x|mu, sigma)
    override def logValue(theta: MALASample): Double = {
        val notmalLogDensity = NormalGaussianLogLikelihood(theta, data)
        notmalLogDensity.value
    }

    override def gradient(theta: MALASample): MALASample = {
        val g = NormalGaussianLogLikelihood(theta, data)

        val params = theta.parameters.copy(mu = g.gradients._1, sigma = g.gradients._2)
        theta.copy(parameters = params)
    }

}

case class NormalGaussianLogLikelihood(theta: MALASample, data: Seq[Double]) {
    private val mu = Var(theta.parameters.mu)
    private val sigma = Var(theta.parameters.sigma)
    private val logNormalizer = 0.5 * log(sqrt(2.0 * scala.math.Pi)) + log(sigma)
    private var compGraph: Node[Scalar, Double] = Var(0.0)

    for (x <- data) yield {
        val f = 0.5 * Neg(pow(x - mu, 2) / sigma) - logNormalizer
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
        compGraph.grad()
        val dfdmu = mu.gradient.unwrap.data.asInstanceOf[Double]
        val dfdsigma = sigma.gradient.unwrap.data.asInstanceOf[Double]
        (dfdmu, dfdsigma)
    }
}

// Gaussian priors on the parameters. Also the prior needs to have a gradient
object MALAPriorEvaluator
    extends DistributionEvaluator[MALASample]
        with GradientEvaluator[MALASample] {

    val priorDistMu = breeze.stats.distributions.Gaussian(0, 10)
    val priorDistSigma = breeze.stats.distributions.Gaussian(0, 20)

    override def logValue(theta: MALASample): Double = {
        priorDistMu.logPdf(theta.parameters.mu) + priorDistSigma.logPdf(theta.parameters.sigma)
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
        val posteriorEvaluator = ProductEvaluator.withGradient(MALAPriorEvaluator, MALALikelihoodEvaluator(data))

        // Using the new Mala proposal as a gradient
        val generator = MalaProposal(posteriorEvaluator, 1e-3, AllParamLens)

        // From here on, the rest ist standard MH sampling

        val chain = MetropolisHastings(generator, posteriorEvaluator)

        val initialSample = MALASample(MALAParameters(0, 1), generatedBy = "initial")
        val malaLogger = new MalaLogger()
        val mhIterator = chain.iterator(initialSample, malaLogger)
        val samples = mhIterator.drop(1000).take(10000).toIndexedSeq

        val estimatedMean = samples.map(sample => sample.parameters.mu).sum / samples.size
        val estimatedSigma = samples.map(sample => sample.parameters.sigma).sum / samples.size

        println((estimatedMean, estimatedSigma))
    }
}

case class MalaLogger() extends AcceptRejectLogger[MALASample] {
    private val numAccepted = collection.mutable.Map[String, Int]()
    private val numRejected = collection.mutable.Map[String, Int]()
    private val verbosePrintLogger = new VerbosePrintLogger[MALASample](Console.out, "")

    override def accept(current: MALASample,
                        sample: MALASample,
                        generator: ProposalGenerator[MALASample],
                        evaluator: DistributionEvaluator[MALASample]
                       ): Unit = {
        val numAcceptedSoFar = numAccepted.getOrElseUpdate(sample.generatedBy, 0)
        numAccepted.update(sample.generatedBy, numAcceptedSoFar + 1)
        verbosePrintLogger.accept(current, sample, generator, evaluator)
    }

    override def reject(current: MALASample,
                        sample: MALASample,
                        generator: ProposalGenerator[MALASample],
                        evaluator: DistributionEvaluator[MALASample]
                       ): Unit = {
        val numRejectedSoFar = numRejected.getOrElseUpdate(sample.generatedBy, 0)
        numRejected.update(sample.generatedBy, numRejectedSoFar + 1)
        //        verbosePrintLogger.reject(current, sample, generator, evaluator)
    }


    def acceptanceRatios(): Map[String, Double] = {
        val generatorNames = numRejected.keys.toSet.union(numAccepted.keys.toSet)
        val acceptanceRatios = for (generatorName <- generatorNames) yield {
            val total = (numAccepted.getOrElse(generatorName, 0)
                + numRejected.getOrElse(generatorName, 0)).toDouble
            (generatorName, numAccepted.getOrElse(generatorName, 0) / total)
        }
        acceptanceRatios.toMap
    }
}
