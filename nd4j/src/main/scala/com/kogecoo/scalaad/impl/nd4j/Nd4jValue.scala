package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.{BooleanTensor, BooleanValue, Tensor, Value}
import org.nd4j.linalg.api.ndarray.INDArray


trait Nd4jValue {

  implicit val value_nd4j_tensor_double: Value[INDArray] = {
    new Value[INDArray] {
      def value(t: Tensor): INDArray = t match {
        case t: Nd4jVector => t.data
      }
    }
  }

  implicit val value_nd4j_scalar_double: Value[T0] = {
    new Value[T0] {
      def value(t: Tensor): T0 = t match {
        case Nd4jScalar(data) => data
      }
    }
  }

  implicit val value_nd4j_tensor_bool: BooleanValue[INDArray] = {
    new BooleanValue[INDArray] {
      def value(t: BooleanTensor): INDArray = t match {
        case t: Nd4jBooleanVector => t.data
      }
    }
  }

  implicit val value_nd4j_scalar_bool: BooleanValue[B0] = {
    new BooleanValue[B0] {

      def value(t: BooleanTensor): B0 = t match {
        case Nd4jBooleanScalar(data) => data
      }
    }
  }

}
