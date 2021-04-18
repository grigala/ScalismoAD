//package ch.grigala.scalismoad.example
//
//import breeze.linalg.DenseVector
//import ch.grigala.scalismoad.sampling.SampleLens
//import ch.grigala.scalismoad.sampling.proposals.GaussianRandomWalkProposal
//import scalismo.sampling.proposals.MixtureProposal
//import scalismo.sampling.DistributionEvaluator
//import scalismo.sampling.algorithms.MetropolisHastings
//
//
//// Parameter class - This is generic and every application chooses its
//// own structures of the parameters
//case class Parameters(mu: Double, sigma: Double)
//case class Sample(parameters: Parameters, generatedBy: String)
//
//
//// The lense for accessing the mean from the parameter vector
//object MeanParamLens extends SampleLens[Sample] {
//
//    override def numberOfParameters: Int = 1
//
//    override def getAsVector(sample: Sample): DenseVector[Double] = {
//        val v = DenseVector.zeros[Double](1)
//        v(0) = sample.parameters.mu
//        v
//    }
//    override def setFromVector(sample: Sample, vector: DenseVector[Double], generatedBy: Option[String]): Sample = {
//        sample.copy(parameters = sample.parameters.copy(mu = vector(0)))
//    }
//}
//
//// The lense for accessing the staddev from the parameter vector
//object StddevParamLens extends SampleLens[Sample] {
//
//    override def numberOfParameters: Int = 1
//
//    override def getAsVector(sample: Sample): DenseVector[Double] = {
//        val v = DenseVector.zeros[Double](1)
//        v(0) = sample.parameters.sigma
//        v
//    }
//
//    override def setFromVector(sample: Sample, vector: DenseVector[Double], generatedBy: Option[String]): Sample = {
//        sample.copy(parameters = sample.parameters.copy(sigma = vector(0)))
//    }
//}
//
//case class Evaluator(data: Seq[Double]) extends DistributionEvaluator[Sample] {
//
//    override def logValue(theta: Sample): Double = {
//        val likelihood = breeze.stats.distributions.Gaussian(
//            theta.parameters.mu,
//            theta.parameters.sigma
//        )
//        val likelihoods = for (x <- data) yield {
//            likelihood.logPdf(x)
//        }
//        likelihoods.sum
//    }
//}
//
//object SampleLensExample {
//
//    def generateSyntheticData(numDataPoints: Int): Seq[Double] = {
//        // The true distribution to sample from
//        val mu = -1
//        val sigma = 3
//
//        val trueDistribution = breeze.stats.distributions.Gaussian(mu, sigma)
//        for (_ <- 0 until numDataPoints) yield {
//            trueDistribution.draw()
//        }
//    }
//
//    def main(args: Array[String]): Unit = {
//
//        scalismo.initialize()
//        implicit val rng = scalismo.utils.Random(42)
//
//        val data = generateSyntheticData(100)
//        val evaluator = Evaluator(data)
//
//        // Using the new Mala proposal as a gradient
//        val generatorForMean = GaussianRandomWalkProposal(1, MeanParamLens)
//        val generatorForStddev = GaussianRandomWalkProposal(0.1, StddevParamLens)
//        val generator = MixtureProposal.fromProposalsWithTransition((0.5, generatorForMean), (0.5, generatorForStddev))
//
//        val chain = MetropolisHastings(generator, evaluator)
//
//        val initialSample = Sample(Parameters(0, 1), generatedBy = "initial")
//        val mhIterator = chain.iterator(initialSample)
//        val samples = mhIterator.drop(1000).take(100000).toIndexedSeq
//
//        val estimatedMean = samples.map(sample => sample.parameters.mu).sum / samples.size
//        val estimatedSigma = samples.map(sample => sample.parameters.sigma).sum / samples.size
//
//        println((estimatedMean, estimatedSigma))
//    }
//}
