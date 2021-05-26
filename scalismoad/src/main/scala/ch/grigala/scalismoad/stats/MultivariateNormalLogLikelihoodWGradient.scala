package ch.grigala.scalismoad.stats

import breeze.linalg.{DenseMatrix, DenseVector, det, diag}
import ch.grigala.scalismoad.graph.{Node, Var, dot, log, pow, sqrt}
import ch.grigala.scalismoad.rule.BreezeRule.Implicits._
import ch.grigala.scalismoad.value.{ContainerValue, Value}
import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.PointDistributionModel

case class MultivariateNormalLogLikelihoodWGradient(model: PointDistributionModel[_3D, TriangleMesh],
                                                    coeffs: DenseVector[Double]) {

    private val rank = model.gp.rank
    private val mean = DenseVector.zeros[Double](rank)
    private val cov = diag(DenseVector.ones[Double](rank))

    private val dim = mean.size
    private lazy val covInv: DenseMatrix[Double] = breeze.linalg.pinv(cov.map(_.toDouble))
    private lazy val covDet: Double = det(cov.map(_.toDouble))

    // Computational graph components
    private val meanVar = Var(mean)
    private val covInvVar = Var(covInv)
    private val coeffsVar = Var(coeffs)

    def compGraph = {
        if (coeffs.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${coeffs.size} should be $dim)")
        val normFactor = pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / sqrt(covDet + 1e-10)

        val x0 = coeffsVar - meanVar
        val x0Val = Var(x0.apply().unwrapContainerValue.asDenseMatrix)
        val x1 = x0Val * covInvVar

        val x1Val = Var(x1.apply().unwrapContainerValue.toDenseVector)
        val exponent = dot(x0, x1Val)
        val ex: Double = -0.5 * exponent.apply().unwrapNonContainerValue
        log(normFactor) + ex
        x1
    }

    def value: Double = {
        compGraph.apply().unwrapNonContainerValue
    }

    def gradients: DenseVector[Double] = {
        compGraph.grad()
        coeffsVar.gradient.unwrapContainerValue
    }
    //    private val rank = model.rank
    //    private val mean = DenseVector.zeros[Double](rank)
    //    private val cov = diag(DenseVector.ones[Double](rank))
    //
    //    require(cov.rows == cov.cols)
    //    require(mean.size == cov.rows)
    //
    //    private val dim = mean.size
    //    private lazy val covInv = breeze.linalg.pinv(cov.map(_.toDouble))
    //    private lazy val covDet = det(cov.map(_.toDouble))
    //
    //    private val m = Var(mean)
    //    private val c = Var(cov)
    //
    //    def compGraph = {
    //        if (coeffs.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${coeffs.size} should be $dim)")
    //        val normFactor = pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / sqrt(covDet + 1e-10)
    //
    //        val x0 = (coeffs - mean).map(identity)
    //        val exponent = -0.5f * x0.dot(covInv * x0)
    //        log(normFactor) + exponent
    //    }
    //
    //    def value = {
    //        compGraph.apply().unwrap.data.asInstanceOf[Double]
    //    }
}
