package ch.grigala.scalismoad.example.fitting

import breeze.linalg.{DenseMatrix, DenseVector}
import ch.grigala.scalismoad.example.fitting.Data.SampleLF
import scalismo.geometry.EuclideanVector
import scalismo.sampling.{ProposalGenerator, TransitionProbability}
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.utils.Random.implicits._

object Proposals {

    case class ShapeUpdateProposal(paramVectorSize: Int, stddev: Double)
        extends ProposalGenerator[SampleLF]
            with TransitionProbability[SampleLF] {

        val perturbationDistr = new MultivariateNormalDistribution(
            DenseVector.zeros(paramVectorSize),
            DenseMatrix.eye[Double](paramVectorSize) * stddev * stddev
        )

        override def propose(sample: SampleLF): SampleLF = {
            val perturbation = perturbationDistr.sample()
            val newParametersLF =
                sample.parameters.copy(modelCoefficients = sample.parameters.modelCoefficients + perturbationDistr.sample)
            sample.copy(generatedBy = s"ShapeUpdateProposal ($stddev)", parameters = newParametersLF)
        }

        override def logTransitionProbability(from: SampleLF, to: SampleLF) = {
            val residual = to.parameters.modelCoefficients - from.parameters.modelCoefficients
            perturbationDistr.logpdf(residual)
        }
    }

    case class RotationUpdateProposal(stddev: Double)
        extends ProposalGenerator[SampleLF]
            with TransitionProbability[SampleLF] {
        val perturbationDistr =
            new MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * stddev * stddev)

        def propose(sample: SampleLF): SampleLF = {
            val perturbation = perturbationDistr.sample
            val newRotationParametersLF = (
                sample.parameters.rotationParametersLF._1 + perturbation(0),
                sample.parameters.rotationParametersLF._2 + perturbation(1),
                sample.parameters.rotationParametersLF._3 + perturbation(2)
            )
            val newParametersLF = sample.parameters.copy(rotationParametersLF = newRotationParametersLF)
            sample.copy(generatedBy = s"RotationUpdateProposal ($stddev)", parameters = newParametersLF)
        }

        override def logTransitionProbability(from: SampleLF, to: SampleLF) = {
            val residual = DenseVector(
                to.parameters.rotationParametersLF._1 - from.parameters.rotationParametersLF._1,
                to.parameters.rotationParametersLF._2 - from.parameters.rotationParametersLF._2,
                to.parameters.rotationParametersLF._3 - from.parameters.rotationParametersLF._3
            )
            perturbationDistr.logpdf(residual)
        }
    }

    case class TranslationUpdateProposal(stddev: Double)
        extends ProposalGenerator[SampleLF]
            with TransitionProbability[SampleLF] {

        val perturbationDistr =
            new MultivariateNormalDistribution(DenseVector.zeros(3), DenseMatrix.eye[Double](3) * stddev * stddev)

        def propose(sample: SampleLF): SampleLF = {
            val newTranslationParametersLF = sample.parameters.translationParametersLF + EuclideanVector.fromBreezeVector(
                perturbationDistr.sample()
            )
            val newParametersLF = sample.parameters.copy(translationParametersLF = newTranslationParametersLF)
            sample.copy(generatedBy = s"TranlationUpdateProposal ($stddev)", parameters = newParametersLF)
        }

        override def logTransitionProbability(from: SampleLF, to: SampleLF): Double = {
            val residual = to.parameters.translationParametersLF - from.parameters.translationParametersLF
            perturbationDistr.logpdf(residual.toBreezeVector)
        }
    }

}
