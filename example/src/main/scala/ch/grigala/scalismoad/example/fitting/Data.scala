package ch.grigala.scalismoad.example.fitting

import breeze.linalg.DenseVector
import scalismo.geometry.{EuclideanVector, Point, _3D}
import scalismo.transformations.{Rotation3D, Translation3D, TranslationAfterRotation, TranslationAfterRotation3D}

object Data {

    case class ParametersLF(translationParametersLF: EuclideanVector[_3D],
                            rotationParametersLF: (Double, Double, Double),
                            modelCoefficients: DenseVector[Double])

    case class SampleLF(generatedBy: String, parameters: ParametersLF, rotationCenter: Point[_3D]) {
        def poseTransformation: TranslationAfterRotation[_3D] = {
            val translation = Translation3D(parameters.translationParametersLF)
            val rotation = Rotation3D(
                parameters.rotationParametersLF._1,
                parameters.rotationParametersLF._2,
                parameters.rotationParametersLF._3,
                rotationCenter
            )
            TranslationAfterRotation3D(translation, rotation)
        }
    }
}
