package com.kogecoo.scalaad.test.graph.op.spec

import ch.grigala.ad.graph.{Node, Pos, Scalar}
import ch.grigala.ad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.ScalarIntCompareRule
import com.kogecoo.scalaad.test.helper.rule.ScalarIntValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpExpectedBehaviorDef, UnaryOpSpec}
import org.scalacheck.Properties


object PosSpecScalarInt extends Properties("Pos - Scalar[Int]") {

  implicit val compareRule = new ScalarIntCompareRule

  val nodeGen = new ScalarIntNodeGen
  val valueGen = new ScalarIntValueGen
  val expects = new PosScalarIntExpectedBehavior

  val scalarIntSpecGen = new UnaryOpSpec[Scalar, Int](expects, nodeGen, valueGen)

  property("+a apply") = scalarIntSpecGen.applyScalar()
  property("+a apply") = scalarIntSpecGen.applyContainer()
  property("+a apply") = scalarIntSpecGen.applyVar()

  property("+a (scalar) w.r.t. unknown var") = scalarIntSpecGen.derivScalarWrtUnknownVar()
  property("+a (container) w.r.t. unknown var") = scalarIntSpecGen.derivContainerWrtUnknownVar()
  property("+a (var) w.r.t. a") = scalarIntSpecGen.derivVarWrtSelf()
  property("+a (var) w.r.t. unknonw var") = scalarIntSpecGen.derivVarWrtUnknownVar()

  property("+a (scalar) propagete with value") = scalarIntSpecGen.propagateScalarWithNCValue()
  property("+a (scalar) propagate with container") = scalarIntSpecGen.propagateScalarWithCValue()
  property("+a (container) propagete with value") = scalarIntSpecGen.propagateContainerWithNCValue()
  property("+a (container) propagate with container") = scalarIntSpecGen.propagateContainerWithCValue()
  property("+a (var) propagete with value") = scalarIntSpecGen.propagateVarWithNCValue()
  property("+a (var) propagate with container") = scalarIntSpecGen.propagateVarWithCValue()

  property("+a (scalar) grad") = scalarIntSpecGen.gradScalar()
  property("+a (container) grad") = scalarIntSpecGen.gradContainer()
  property("+a (var) grad") = scalarIntSpecGen.gradVar()


}

class PosScalarIntExpectedBehavior(implicit vr: ValueRule[Scalar, Int]) extends UnaryOpExpectedBehaviorDef[Scalar, Int] {

  override val zero: Int = 0

  override def zero(shape: Scalar[Int]): Scalar[Int] = Scalar(0)

  override def op(node: Node[Scalar, Int]): Node[Scalar, Int] = Pos(node)

  override def applyScalar(a: Int): Int = +a

  override def applyContainer(a: Scalar[Int]): Scalar[Int] = Scalar(+a.data)

  override def applyVar(a: Scalar[Int]): Scalar[Int] = Scalar(+a.data)

  override def derivVarWrtSelf(a: Scalar[Int]): Scalar[Int] = Scalar(1)

  override def propagateVarWithNCValue(a: Scalar[Int], b: Int): Scalar[Int] = Scalar(b)

  override def propagateVarWithCValue(a: Scalar[Int], b: Scalar[Int]): Scalar[Int] = b

  override def gradVar(a: Scalar[Int]): Scalar[Int] = Scalar(1)

}
