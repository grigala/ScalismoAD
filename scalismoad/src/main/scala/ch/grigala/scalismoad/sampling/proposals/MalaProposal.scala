package ch.grigala.scalismoad.sampling.proposals

import breeze.linalg.DenseVector
import ch.grigala.scalismoad.sampling.SampleLens
import scalismo.sampling.{DistributionEvaluator, GradientEvaluator, ProposalGenerator, TransitionProbability}
import scalismo.utils.Memoize

/**
 * An implmentation of the Langevin Monte carlo as a proposal. Used with the
 * Metropolis-Hastings algorithm, it leads to the Metropolis-Adjusted Langevin Algorithm (MALA)
 * as described [here](https://en.wikipedia.org/wiki/Metropolis-adjusted_Langevin_algorithm).
 *
 * @param evaluator The probability density function to sample from
 * @param tau The time step to take in each iteration
 * @param sampleLens: A lense, which determines which part of the sample to consider for this proposal
 *
 */
case class MalaProposal[A](evaluator: DistributionEvaluator[A] with GradientEvaluator[A],
                           tau: Double,
                           sampleLens: SampleLens[A, DenseVector[Double]])(implicit rng: scalismo.utils.Random)
    extends ProposalGenerator[A]
        with TransitionProbability[A] {

    private val evaluatorGradient = Memoize(evaluator.gradient, 10)

    override def propose(sample: A): A = {

        val gradient = sampleLens.get(evaluatorGradient(sample))
        val perturbation = DenseVector.rand(gradient.length, rng.breezeRandBasis.gaussian) * Math.sqrt(2.0 * tau)
        val newParameters = sampleLens.get(sample) + gradient * tau + perturbation
        sampleLens.replace(sample, newParameters, Some(s"MalaProposal (tau=$tau)"))
    }

    override def logTransitionProbability(from: A, to: A): Double = {
        val g = sampleLens.get(evaluatorGradient(from))
        Math.exp(breeze.linalg.norm(sampleLens.get(to) - sampleLens.get(from) - g * tau, 2) * (-1.0 / (4 * tau)))
    }
}
