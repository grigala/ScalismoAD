package ch.grigala.scalismoad.stats

import breeze.linalg.{DenseMatrix, DenseVector, det, diag}
import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.rule.BreezeRule.Implicits._
import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.PointDistributionModel

case class MultivariateGaussianLogLikelihoodWGradient(model: PointDistributionModel[_3D, TriangleMesh],
                                                      coeffs: DenseVector[Double]) {
    private val rank = model.rank
    private val mean = DenseVector.zeros[Double](rank)
    private val cov = diag(DenseVector.ones[Double](rank))

    require(cov.rows == cov.cols)
    require(mean.size == cov.rows)

    private val dim = mean.size
    private lazy val covInv = breeze.linalg.pinv(cov.map(_.toDouble))
    private lazy val covDet = det(cov.map(_.toDouble))

    private val m = Var(mean)
    private val c = Var(cov)

    def compGraph: Node[DenseVector, Double] = {
        if (coeffs.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${coeffs.size} should be $dim)")
        val normFactor = pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / sqrt(covDet + 1e-10)

        val x0 = (coeffs - mean).map(identity)
        val exponent = -0.5f * x0.dot(covInv * x0)
        log(normFactor) + exponent
    }

    def value: Double = {
        compGraph.apply().unwrap.data.asInstanceOf[Double]
    }

    def gradients: (DenseVector[Double], DenseMatrix[Double]) = {
        compGraph.grad()
        (m.gradient.unwrap.data.asInstanceOf[DenseVector[Double]],
            c.gradient.unwrap.data.asInstanceOf[DenseMatrix[Double]])
    }


}
