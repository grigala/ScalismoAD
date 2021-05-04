package ch.grigala.scalismoad.example.fitting

import ch.grigala.scalismoad.example.fitting.Data.SampleLF
import ch.grigala.scalismoad.example.fitting.Utils.marginalizeModelForCorrespondences
import scalismo.common.PointId
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.sampling.DistributionEvaluator
import scalismo.statisticalmodel.{MultivariateNormalDistribution, PointDistributionModel}
import scalismo.utils.Memoize

object Evaluators {

    case class PriorEvaluator(model: PointDistributionModel[_3D, TriangleMesh]) extends DistributionEvaluator[SampleLF] {

        val translationPrior = breeze.stats.distributions.Gaussian(0.0, 5.0)
        val rotationPrior = breeze.stats.distributions.Gaussian(0, 0.1)

        override def logValue(sample: SampleLF): Double = {
            model.gp.logpdf(sample.parameters.modelCoefficients) +
                translationPrior.logPdf(sample.parameters.translationParametersLF.x) +
                translationPrior.logPdf(sample.parameters.translationParametersLF.y) +
                translationPrior.logPdf(sample.parameters.translationParametersLF.z) +
                rotationPrior.logPdf(sample.parameters.rotationParametersLF._1) +
                rotationPrior.logPdf(sample.parameters.rotationParametersLF._2) +
                rotationPrior.logPdf(sample.parameters.rotationParametersLF._3)
        }
    }

    case class CorrespondenceEvaluator(model: PointDistributionModel[_3D, TriangleMesh],
                                       correspondences: Seq[(PointId, Point[_3D], MultivariateNormalDistribution)])
        extends DistributionEvaluator[SampleLF] {


        val (marginalizedModel, newCorrespondences) = marginalizeModelForCorrespondences(model, correspondences)

        override def logValue(sample: SampleLF): Double = {

            val currModelInstance = marginalizedModel
                .instance(sample.parameters.modelCoefficients)
                .transform(sample.poseTransformation)

            val likelihoods = newCorrespondences.map(correspondence => {
                val (id, targetPoint, uncertainty) = correspondence
                val modelInstancePoint = currModelInstance.pointSet.point(id)
                val observedDeformation = targetPoint - modelInstancePoint

                uncertainty.logpdf(observedDeformation.toBreezeVector)
            })

            val loglikelihood = likelihoods.sum
            loglikelihood
        }
    }

    case class CachedEvaluator[A](evaluator: DistributionEvaluator[A]) extends DistributionEvaluator[A] {
        val memoizedLogValue = Memoize(evaluator.logValue, 10)

        override def logValue(sample: A): Double = {
            memoizedLogValue(sample)
        }
    }

}
