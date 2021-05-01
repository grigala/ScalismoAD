//package ch.grigala.scalismoad.example
//
//import breeze.numerics.constants.Pi
//import breeze.numerics.{log, sqrt}
//import breeze.stats.meanAndVariance
//import ch.grigala.scalismoad.data.{Parameters, Sample}
//import ch.grigala.scalismoad.graph.{Neg, Var, pow}
//import ch.grigala.scalismoad.logging.Logger
//import ch.grigala.scalismoad.sampling.evaluators.ProductEvaluator
//import scalismo.sampling.algorithms.MetropolisHastings
//import scalismo.sampling.proposals.MixtureProposal
//import scalismo.sampling.{DistributionEvaluator, GradientEvaluator, ProposalGenerator, TransitionProbability}
//
//// https://scalismo.org/docs/tutorials/tutorial14
//object BasicFramework2 extends App {
//    scalismo.initialize()
//    implicit val rng = scalismo.utils.Random(42)
//
//    val a = 0.2
//    val b = 3
//    val sigma2 = 0.5
//    val errorDist = breeze.stats.distributions.Gaussian(0, sigma2)
//    val data = for (x <- 0 until 100) yield {
//        (x.toDouble, a * x + b + errorDist.draw())
//    }
//
//    val posteriorEvaluator = ProductEvaluator(PriorEvaluator2, LikelihoodEvaluator2(data))
//
//    val smallStepProposal = RandomWalkProposal2(0.01, 0.01, 0.01)
//    val largeStepProposal = RandomWalkProposal2(0.1, 0.1, 0.1)
//
//    val generator = MixtureProposal.fromProposalsWithTransition[Sample](
//        (0.8, smallStepProposal),
//        (0.2, largeStepProposal)
//    )
//
//    val chain = MetropolisHastings(generator, posteriorEvaluator)
//
//    val initialSample = Sample(Parameters(0.0, 0.0, 1.0), generatedBy = "initial")
//    val mhIterator = chain.iterator(initialSample)
//    val samples = mhIterator.drop(5000).take(15000).toIndexedSeq
//
//
//    val logger = new Logger()
//    val mhIteratorWithLogging = chain.iterator(initialSample, logger)
//
//    val samples2 = mhIteratorWithLogging.drop(5000).take(15000).toIndexedSeq
//
//    val meanAndVarianceA = meanAndVariance(samples.map(_.parameters.a))
//    println(s"Estimates for parameter a: mean = ${meanAndVarianceA.mean}, var = ${meanAndVarianceA.variance}")
//    val meanAndVarianceB = meanAndVariance(samples.map(_.parameters.b))
//    println(s"Estimates for parameter b: mean = ${meanAndVarianceB.mean}, var = ${meanAndVarianceB.variance}")
//    val meanAndVarianceSigma2 = meanAndVariance(samples.map(_.parameters.sigma2))
//    println(s"Estimates for parameter sigma2: mean = ${meanAndVarianceSigma2.mean}, var = ${meanAndVarianceSigma2.variance}")
//
//    println("acceptance ratio is " + logger.acceptanceRatios())
//}
//
//case class LikelihoodEvaluator2(data: Seq[(Double, Double)]) extends DistributionEvaluator[Sample] {
//
//
//    import ch.grigala.scalismoad.rule.ScalarRule.Implicits._
//
//    override def logValue(theta: Sample): Double = {
//
//        val likelihoods = for ((x, y) <- data) yield {
//            val likelihood = breeze.stats.distributions.Gaussian(
//                theta.parameters.a * x + theta.parameters.b, theta.parameters.sigma2)
//
//            println("=" * 50)
//            val a = Var(theta.parameters.a)
//            val b = Var(theta.parameters.b)
//            val sig = Var(theta.parameters.sigma2)
//
//            val f = Neg(pow(y - (a * x + b), 2) / sig)
//
//            // Manual calculation of the logPdf mirrors Gaussian implementation
//            val ff = (y - (a * x + b)) / sig // same as (t - mu) / sigma
//            val d = ff.apply().unwrap.data.asInstanceOf[Double]
//            val unnormalizedLogDensity = (-d * d) / 2.0 // e^{-1/2 (x-mu/sigma)^2}
//            val logNormalizer = log(sqrt(2 * Pi)) + log(theta.parameters.sigma2) // 1/(sigma*sqrt(2*PI))
//
//            // The true unnormalized value of the f given a, b, sigma2 and observations x & y
//            val pure_value = f.apply().unwrap.data.asInstanceOf[Double]
//            val normalizedFValue= pure_value / 2.0 - logNormalizer
//
//            println(s"density of f(x, y)=${pure_value}")
//            println(s"normalized density of f(x, y)=$normalizedFValue")
//            println(s"manually calculated logPdf=${unnormalizedLogDensity - logNormalizer}")
//            println(s"breeze: unnormalizedLogPdf=${likelihood.unnormalizedLogPdf(y)}, logNormalizer=${likelihood.logNormalizer}")
//            println(s"x=$x, y=$y, theta=$theta, logPdf(y)=${likelihood.logPdf(y)}")
////            likelihood.logPdf(y)
//            normalizedFValue
//
////            logPdf = unnormalizedLogPdf(y) - logNormalizer
////            override def unnormalizedLogPdf(t: Double) = {
////                val d = (t - mu) / sigma
////                -d * d / 2.0
////            }
////            lazy val logNormalizer = log(sqrt(2 * Pi)) + log(sigma)
//        }
//        likelihoods.sum
//    }
//}
//
//object PriorEvaluator2 extends DistributionEvaluator[Sample] with GradientEvaluator[Sample] {
//
//    val priorDistA = breeze.stats.distributions.Gaussian(0, 1)
//    val priorDistB = breeze.stats.distributions.Gaussian(0, 10)
//    val priorDistSigma = breeze.stats.distributions.LogNormal(0, 0.25)
//
//    override def logValue(theta: Sample): Double = {
//        priorDistA.logPdf(theta.parameters.a)
//        +priorDistB.logPdf(theta.parameters.b)
//        +priorDistSigma.logPdf(theta.parameters.sigma2)
//    }
//
//    override def gradient(sample: Sample): Sample = ???
//}
//
//case class RandomWalkProposal2(stepLengthA: Double, stepLengthB: Double, stepLengthSigma2: Double)(implicit rng: scalismo.utils.Random)
//    extends ProposalGenerator[Sample] with TransitionProbability[Sample] {
//
//    override def propose(sample: Sample): Sample = {
//        val newParameters = Parameters(
//            a = sample.parameters.a + rng.breezeRandBasis.gaussian(0, stepLengthA).draw(),
//            b = sample.parameters.b + rng.breezeRandBasis.gaussian(0, stepLengthB).draw(),
//            sigma2 = sample.parameters.sigma2 + rng.breezeRandBasis.gaussian(0, stepLengthSigma2).draw(),
//        )
//
//        Sample(newParameters, s"RandomWalkProposal2 ($stepLengthA, $stepLengthB)")
//    }
//
//    override def logTransitionProbability(from: Sample, to: Sample): Double = {
//
//        val stepDistA = breeze.stats.distributions.Gaussian(0, stepLengthA)
//        val stepDistB = breeze.stats.distributions.Gaussian(0, stepLengthB)
//        val stepDistSigma2 = breeze.stats.distributions.Gaussian(0, stepLengthSigma2)
//        val residualA = to.parameters.a - from.parameters.a
//        val residualB = to.parameters.b - from.parameters.b
//        val residualSigma2 = to.parameters.sigma2 - from.parameters.sigma2
//        stepDistA.logPdf(residualA) + stepDistB.logPdf(residualB) + stepDistSigma2.logPdf(residualSigma2)
//    }
//}
