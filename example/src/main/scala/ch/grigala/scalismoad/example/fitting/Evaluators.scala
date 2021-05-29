package ch.grigala.scalismoad.example.fitting

import ch.grigala.scalismoad.Utils.~=
import ch.grigala.scalismoad.example.fitting.Data.SampleLF
import ch.grigala.scalismoad.example.fitting.Utils.marginalizeModelForCorrespondences
import ch.grigala.scalismoad.stats.{MultivariateNormalLogLikelihoodWGradient, UnivariateNormalLogLikelihoodWGradient}
import scalismo.common.PointId
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.sampling.{DistributionEvaluator, GradientEvaluator}
import scalismo.statisticalmodel.{MultivariateNormalDistribution, PointDistributionModel}
import scalismo.utils.Memoize
import shapeless.syntax.std.tuple.productTupleOps

object Evaluators {

    case class PriorEvaluator(model: PointDistributionModel[_3D, TriangleMesh]) extends DistributionEvaluator[SampleLF]
        with GradientEvaluator[SampleLF] {

        // Remove after test is passing consistently
        val translationPrior = breeze.stats.distributions.Gaussian(0.0, 5.0)
        val rotationPrior = breeze.stats.distributions.Gaussian(0, 0.1)

        override def logValue(sample: SampleLF): Double = {
            // Translation prior logpdf for each parameter
            val tpSum = UnivariateNormalLogLikelihoodWGradient(0.0, 5.0, sample.parameters.translationParametersLF.toArray)
            // Rotation prior logpdf for each parameter
            val rpSum = UnivariateNormalLogLikelihoodWGradient(0.0, 0.1, sample.parameters.rotationParametersLF.toArray)

            // model coefficients logpdf
            val modelCoeffSum = MultivariateNormalLogLikelihoodWGradient(model, sample.parameters.modelCoefficients)

            // Just for testing
            val testTranslationSum = translationPrior.logPdf(sample.parameters.translationParametersLF.x) +
                translationPrior.logPdf(sample.parameters.translationParametersLF.y) +
                translationPrior.logPdf(sample.parameters.translationParametersLF.z)

            val testRotationSum = rotationPrior.logPdf(sample.parameters.rotationParametersLF._1) +
                rotationPrior.logPdf(sample.parameters.rotationParametersLF._2) +
                rotationPrior.logPdf(sample.parameters.rotationParametersLF._3)

            val testModelCoeffs = model.gp.logpdf(sample.parameters.modelCoefficients)

            ~=(tpSum.value, testTranslationSum, 1e-10)
            ~=(rpSum.value, testRotationSum, 1e-10)
            ~=(modelCoeffSum.value, testModelCoeffs, 1e-10)

            modelCoeffSum.value + tpSum.value + rpSum.value
        }

        override def gradient(sample: SampleLF): SampleLF = {
            sample.copy()
        }
    }


    case class CorrespondenceEvaluator(model: PointDistributionModel[_3D, TriangleMesh],
                                       correspondences: Seq[(PointId, Point[_3D], MultivariateNormalDistribution)])
        extends DistributionEvaluator[SampleLF] with GradientEvaluator[SampleLF] {


        val (marginalizedModel, newCorrespondences) = marginalizeModelForCorrespondences(model, correspondences)

        override def logValue(sample: SampleLF): Double = {

            val currModelInstance = marginalizedModel
                .instance(sample.parameters.modelCoefficients)
                .transform(sample.poseTransformation)

            val likelihoods = newCorrespondences.map(correspondence => {
                val (id, targetPoint, uncertainty) = correspondence
                val modelInstancePoint = currModelInstance.pointSet.point(id)
                val observedDeformation = targetPoint - modelInstancePoint
                val logpdf = uncertainty.logpdf(observedDeformation.toBreezeVector)
                logpdf
            })

            val loglikelihood = likelihoods.sum
            loglikelihood
        }

        override def gradient(sample: SampleLF): SampleLF = ???
    }

    case class CachedEvaluator[A](evaluator: DistributionEvaluator[A] with GradientEvaluator[A])
        extends DistributionEvaluator[A] with GradientEvaluator[A] {
        val memoizedLogValue = Memoize(evaluator.logValue, 10)
        val memoizedGradientValue = Memoize(evaluator.gradient, 10)

        override def logValue(sample: A): Double = {
            memoizedLogValue(sample)
        }

        override def gradient(sample: A): A = {
            memoizedGradientValue(sample)
        }
    }

}
