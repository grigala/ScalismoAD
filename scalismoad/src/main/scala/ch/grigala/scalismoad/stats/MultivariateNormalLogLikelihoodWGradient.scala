package ch.grigala.scalismoad.stats

import breeze.linalg.{DenseMatrix, DenseVector, det, diag}
import ch.grigala.scalismoad.graph.{Node, Scalar, ScalarConst, Var, dot, log, pow, sqrt}
import ch.grigala.scalismoad.rule.BreezeRule.Implicits._
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
    private val meanVar: Var[DenseMatrix, Double] = Var(mean.toDenseMatrix)
    private val covInvVar: Node[DenseMatrix, Double] = Var(covInv)
    private val coeffsVar: Var[DenseMatrix, Double] = Var(coeffs.toDenseMatrix)
    private val n: Node[DenseMatrix, Double] = Var(DenseVector[Double](-0.5).toDenseMatrix)

    def compGraph = {
        if (coeffs.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${coeffs.size} should be $dim)")
        val normFactor = pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / sqrt(covDet + 1e-10)

        val x0 = coeffsVar - meanVar
        val x1 = covInvVar * x0.T()
        val exponent = n * (x0 * x1) // result is 1x1 matrix
        // FIXME: at the moment we cannot directly add log(normFactor) with exponent without extracting the value first
        // Currently this calculates a correct log value however the gradient calculation correctness needs to be tested
        val extractedNormFactor: Double = log(normFactor).apply().unwrapNonContainerValue
        // this is a hack at the moment
        val normalizer = Var(DenseVector[Double](extractedNormFactor).asDenseMatrix)
        exponent + normalizer
    }

    def value: Double = {
        // Even though the result of compGraph is DenseMatrix, it is a 1x1 matrix and we try to extract this single primal value
        compGraph.apply().unwrapContainerValue.data.apply(0)
    }

    def gradients: DenseVector[Double] = {
        compGraph.grad()
        coeffsVar.gradient.unwrapContainerValue.toDenseVector
    }
}
