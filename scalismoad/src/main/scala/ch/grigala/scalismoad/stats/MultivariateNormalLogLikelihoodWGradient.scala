package ch.grigala.scalismoad.stats

import breeze.linalg.{DenseVector, det, diag}
import ch.grigala.scalismoad.graph._
import ch.grigala.scalismoad.rule.BreezeRule.Implicits._
import scalismo.geometry._3D
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.PointDistributionModel

case class MultivariateNormalLogLikelihoodWGradient(model: PointDistributionModel[_3D, TriangleMesh],
                                                    coeffs: DenseVector[Double]) {

    private val rank = model.rank
    private val mean = DenseVector.zeros[Double](rank)
    private val cov = diag(DenseVector.ones[Double](rank))

    private val dim = mean.size
    private lazy val covInv = breeze.linalg.pinv(cov.map(_.toDouble))
    private lazy val covDet = det(cov.map(_.toDouble))

    private val m = Var(mean)
    private val c = Var(cov)

    //    private val x = Var(coeffs)
    //
    //    def compGraph: Node[DenseVector, Double] = {
    //        if (coeffs.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${coeffs.size} should be $dim)")
    //
    //        val m: Var[DenseVector, Double] = Var(mean)
    //        val cInv: Var[DenseVector, Double] = Var(covInv.toDenseVector)
    //
    //        //https://en.wikipedia.org/wiki/Multivariate_normal_distribution#Density_function
    //        val normFactor = pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / sqrt(covDet + 1e-10)
    //
    //        val x0: Node[DenseVector, Double] = x - m
    //        val x1: Node[DenseVector, Double] = cInv * x0
    //        val x2: Double = -0.5 * dot(x1, x0).apply().unwrap.asInstanceOf[Double]
    //        println(s"${log(normFactor) - x2}")
    //        log(normFactor) - x2
    //    }

    def compGraph: Node[DenseVector, Double] = {
        if (coeffs.size != dim) throw new Exception(s"invalid vector dimensionality (provided ${coeffs.size} should be $dim)")
        val normFactor = pow(2.0 * math.Pi, -dim / 2.0) * 1.0 / sqrt(covDet + 1e-10)

        val x0: DenseVector[Double] = (coeffs - mean).map(identity)
        val exponent: Double = x0.dot(covInv * x0)
        log(normFactor) + exponent
    }

    def value: Double = {
        compGraph.apply().unwrap.data.asInstanceOf[Double]
    }

    def gradients: DenseVector[Double] = {
        compGraph.grad()
        // TODO we probably need gradient of coeffs, to do that coeffs needs to be part of the computational graph as a node
        // The matrix to vector multiplication complicates things a bit int he computational graph
        // as well as the behavior of the dot product
        //        x.gradient.unwrap.data.asInstanceOf[DenseVector[Double]]
        DenseVector.zeros(10)
    }
}
