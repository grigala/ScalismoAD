package ch.grigala.scalismoad.example.fitting

import breeze.linalg.{DenseMatrix, DenseVector}
import ch.grigala.scalismoad.example.fitting.Data.{ParametersLF, SampleLF}
import ch.grigala.scalismoad.example.fitting.Evaluators.{CachedEvaluator, CorrespondenceEvaluator, PriorEvaluator}
import ch.grigala.scalismoad.example.fitting.Proposals.{RotationUpdateProposal, ShapeUpdateProposal, TranslationUpdateProposal}
import ch.grigala.scalismoad.example.fitting.Utils.{computeCovarianceFromSampleLFs, computeMean, marginalizeModelForCorrespondences}
import scalismo.geometry._
import scalismo.io.{LandmarkIO, StatisticalModelIO}
import scalismo.mesh.TriangleMesh
import scalismo.sampling.algorithms.MetropolisHastings
import scalismo.sampling.evaluators.ProductEvaluator
import scalismo.sampling.proposals.MixtureProposal
import scalismo.statisticalmodel.MultivariateNormalDistribution
import scalismo.ui.api.ScalismoUIHeadless
import scalismo.utils.Random.implicits._

object LandmarkFitting extends App {

    scalismo.initialize()

    val ui = ScalismoUIHeadless()

    val model = StatisticalModelIO.readStatisticalTriangleMeshModel3D(new java.io.File("datasets/bfm.h5")).get

    val modelGroup = ui.createGroup("model")
    val modelView = ui.show(modelGroup, model, "model")
    modelView.referenceView.opacity = 0.5

    val modelLms = LandmarkIO.readLandmarksJson[_3D](new java.io.File("datasets/modelLM_mcmc.json")).get
    val modelLmViews = ui.show(modelGroup, modelLms, "modelLandmarks")
    modelLmViews.foreach(lmView => lmView.color = java.awt.Color.BLUE)

    val targetGroup = ui.createGroup("target")

    val targetLms = LandmarkIO.readLandmarksJson3D(new java.io.File("datasets/targetLM_mcmc.json")).get
    val targetLmViews = ui.show(targetGroup, targetLms, "targetLandmarks")
    modelLmViews.foreach(lmView => lmView.color = java.awt.Color.RED)

    val modelLmIds = modelLms.map(l => model.mean.pointSet.pointId(l.point).get)
    val targetPoints = targetLms.map(l => l.point)

    val landmarkNoiseVariance = 9.0
    val uncertainty = MultivariateNormalDistribution(
        DenseVector.zeros[Double](3),
        DenseMatrix.eye[Double](3) * landmarkNoiseVariance
    )

    val correspondences = modelLmIds
        .zip(targetPoints)
        .map(modelIdWithTargetPoint => {
            val (modelId, targetPoint) = modelIdWithTargetPoint
            (modelId, targetPoint, uncertainty)
        })

    val likelihoodEvaluator = CachedEvaluator(CorrespondenceEvaluator(model, correspondences))
    val priorEvaluator = CachedEvaluator(PriorEvaluator(model))

    val posteriorEvaluator = ProductEvaluator(priorEvaluator, likelihoodEvaluator)

    val shapeUpdateProposal = ShapeUpdateProposal(model.rank, 0.1)
    val rotationUpdateProposal = RotationUpdateProposal(0.01)
    val translationUpdateProposal = TranslationUpdateProposal(1.0)
    val generator = MixtureProposal.fromProposalsWithTransition(
        (0.6, shapeUpdateProposal),
        (0.2, rotationUpdateProposal),
        (0.2, translationUpdateProposal)
    )

    def computeCenterOfMass(mesh: TriangleMesh[_3D]): Point[_3D] = {
        val normFactor = 1.0 / mesh.pointSet.numberOfPoints
        mesh.pointSet.points.foldLeft(Point(0, 0, 0))((sum, point) => sum + point.toVector * normFactor)
    }

    val initialParametersLF = ParametersLF(
        EuclideanVector(0, 0, 0),
        (0.0, 0.0, 0.0),
        DenseVector.zeros[Double](model.rank)
    )

    val initialSampleLF = SampleLF("initial", initialParametersLF, computeCenterOfMass(model.mean))

    val chain = MetropolisHastings(generator, posteriorEvaluator)
    val logger = new LoggerLF()
    val mhIterator = chain.iterator(initialSampleLF, logger)

    val samplingIterator = for ((sample, iteration) <- mhIterator.zipWithIndex) yield {
        println("iteration " + iteration)
        if (iteration % 500 == 0) {
            modelView.shapeModelTransformationView.shapeTransformationView.coefficients = sample.parameters.modelCoefficients
            modelView.shapeModelTransformationView.poseTransformationView.transformation = sample.poseTransformation
        }
        sample
    }

    val samples = samplingIterator.drop(1000).take(10000).toIndexedSeq
    println(logger.acceptanceRatios())

    val bestSampleLF = samples.maxBy(posteriorEvaluator.logValue)
    val bestFit = model.instance(bestSampleLF.parameters.modelCoefficients).transform(bestSampleLF.poseTransformation)
    val resultGroup = ui.createGroup("result")
    ui.show(resultGroup, bestFit, "best fit")


    val (marginalizedModel, newCorrespondences) = marginalizeModelForCorrespondences(model, correspondences)
    for ((id, _, _) <- newCorrespondences) {
        val meanPointPosition = computeMean(marginalizedModel, id, samples)
        println(s"expected position for point at id $id  = $meanPointPosition")
        val cov = computeCovarianceFromSampleLFs(marginalizedModel, id, meanPointPosition, samples)
        println(
            s"posterior variance computed  for point at id (shape and pose) $id  = ${cov(0, 0)}, ${cov(1, 1)}, ${cov(2, 2)}"
        )
    }

    val posteriorFixedPose = model.posterior(correspondences.toIndexedSeq)

    for ((id, _, _) <- newCorrespondences) {
        val cov = posteriorFixedPose.cov(id, id)
        println(
            s"posterior variance computed by analytic posterior (shape only) for point with id $id = ${cov(0, 0)}, ${cov(1, 1)}, ${cov(2, 2)}"
        )
    }

}






