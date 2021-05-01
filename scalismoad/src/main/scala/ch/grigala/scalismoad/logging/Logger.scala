package ch.grigala.scalismoad.logging

import ch.grigala.scalismoad.data.Sample
import scalismo.sampling.loggers.AcceptRejectLogger
import scalismo.sampling.{DistributionEvaluator, ProposalGenerator}

class Logger extends AcceptRejectLogger[Sample] {
    private val numAccepted = collection.mutable.Map[String, Int]()
    private val numRejected = collection.mutable.Map[String, Int]()
    private val verbosePrintLogger = new VerbosePrintLogger[Sample](Console.out, "")

    override def accept(current: Sample,
                        sample: Sample,
                        generator: ProposalGenerator[Sample],
                        evaluator: DistributionEvaluator[Sample]
                       ): Unit = {
        val numAcceptedSoFar = numAccepted.getOrElseUpdate(sample.generatedBy, 0)
        numAccepted.update(sample.generatedBy, numAcceptedSoFar + 1)
        verbosePrintLogger.accept(current, sample, generator, evaluator)
    }

    override def reject(current: Sample,
                        sample: Sample,
                        generator: ProposalGenerator[Sample],
                        evaluator: DistributionEvaluator[Sample]
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
