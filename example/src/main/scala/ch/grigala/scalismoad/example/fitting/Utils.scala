package ch.grigala.scalismoad.example.fitting

import ch.grigala.scalismoad.example.fitting.Data.SampleLF
import scalismo.common.{PointId, UnstructuredPointsDomain}
import scalismo.geometry.{EuclideanVector, Point, SquareMatrix, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.{MultivariateNormalDistribution, PointDistributionModel}

object Utils {

    def marginalizeModelForCorrespondences(model: PointDistributionModel[_3D, TriangleMesh],
                                           correspondences: Seq[(PointId, Point[_3D],
                                               MultivariateNormalDistribution)])
    : (PointDistributionModel[_3D, UnstructuredPointsDomain],
        Seq[(PointId, Point[_3D], MultivariateNormalDistribution)]) = {

        val (modelIds, _, _) = correspondences.unzip3
        val marginalizedModel = model.marginal(modelIds.toIndexedSeq)
        val newCorrespondences = correspondences.map(idWithTargetPoint => {
            val (id, targetPoint, uncertainty) = idWithTargetPoint
            val modelPoint = model.reference.pointSet.point(id)
            val newId = marginalizedModel.reference.pointSet.findClosestPoint(modelPoint).id
            (newId, targetPoint, uncertainty)
        })
        (marginalizedModel, newCorrespondences)
    }

    def computeMean(model: PointDistributionModel[_3D, UnstructuredPointsDomain], id: PointId, samples: IndexedSeq[SampleLF]): Point[_3D] = {
        var mean = EuclideanVector(0, 0, 0)
        for (sample <- samples) yield {
            val modelInstance = model.instance(sample.parameters.modelCoefficients)
            val pointForInstance = modelInstance.transform(sample.poseTransformation).pointSet.point(id)
            mean += pointForInstance.toVector
        }
        (mean * 1.0 / samples.size).toPoint
    }

    def computeCovarianceFromSampleLFs(model: PointDistributionModel[_3D, UnstructuredPointsDomain],
                                       id: PointId,
                                       mean: Point[_3D],
                                       samples: IndexedSeq[SampleLF]): SquareMatrix[_3D] = {
        var cov = SquareMatrix.zeros[_3D]
        for (sample <- samples) yield {
            val modelInstance = model.instance(sample.parameters.modelCoefficients)
            val pointForInstance = modelInstance.transform(sample.poseTransformation).pointSet.point(id)
            val v = pointForInstance - mean
            cov += v.outer(v)
        }
        cov * (1.0 / samples.size)
    }
}
