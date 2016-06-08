package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.{BooleanTensor2, Tensor2}


trait StdMatValue {

  implicit val value_stdmat_double: Value[Tensor2, StdMat[Double]] = new Value[Tensor2, StdMat[Double]] {
    def value(t: Tensor2): StdMat[Double] = t match {
      case t => t.toStdDouble
    }
  }

  implicit val value_stdmat_boolean: Value[BooleanTensor2, StdMat[Boolean]] = new Value[BooleanTensor2, StdMat[Boolean]] {
    def value(t: BooleanTensor2): StdMat[Boolean] = t match {
      case t => t.toStd
    }
  }
}