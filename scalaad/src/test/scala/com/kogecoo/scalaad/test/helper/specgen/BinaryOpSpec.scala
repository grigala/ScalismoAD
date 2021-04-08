package com.kogecoo.scalaad.test.helper.specgen

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherProp._
import com.kogecoo.scalaad.test.helper.rule.CompareRule
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll


class BinaryOpSpec[U[_], T](
                               d: BinaryOpExpectedBehaviorDef[U, T],
                               nodes: GenNode[U, T],
                               values: GenValue[U, T]
                           )(implicit rule: ValueRule[U, T], shouldBe: CompareRule[U, T]) {

    type Restriction = T => Boolean

    lazy val default: Restriction = (_: T) => true

    def applyScalarScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(sc(r1), sc(r2)) { case (a: ScalarConstSample[U, T], b: ScalarConstSample[U, T]) =>
            d.op(a.node, b.node).apply() shouldBe d.applyScalarScalar(a.src, b.src)
        }
    }

    def applyScalarContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(sc(r1), cc(r2)) { case (a: ScalarConstSample[U, T], b: ContainerConstSample[U, T]) =>
            d.op(a.node, b.node).apply() shouldBe d.applyScalarContainer(a.src, b.src)
        }
    }

    def applyScalarVar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(sc(r1), varGen(r2)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).apply() shouldBe d.applyScalarVar(a.src, b.src)
        }
    }

    def applyContainerScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(cc(r1), sc(r2)) { case (a: ContainerConstSample[U, T], b: ScalarConstSample[U, T]) =>
            d.op(a.node, b.node).apply() shouldBe d.applyContainerScalar(a.src, b.src)
        }
    }

    def applyContainerContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(cc(r1), cc(r2)) { case (a: ContainerConstSample[U, T], b: ContainerConstSample[U, T]) =>
            d.op(a.node, b.node).apply() shouldBe d.applyContainerContainer(a.src, b.src)
        }
    }

    def applyContainerVar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(cc(r2), varGen(r2)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).apply() shouldBe d.applyContainerVar(a.src, b.src)
        }
    }

    def applyVarScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), sc(r2)) { case (a: VarSample[U, T], b: ScalarConstSample[U, T]) =>
            d.op(a.node, b.node).apply() shouldBe d.applyVarScalar(a.src, b.src)
        }
    }

    def applyVarContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), cc(r2)) { case (a: VarSample[U, T], b: ContainerConstSample[U, T]) =>
            d.op(a.node, b.node).apply() shouldBe d.applyVarContainer(a.src, b.src)
        }
    }

    def applyVarVar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), varGen(r2)) { case (a: VarSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).apply() shouldBe d.applyVarVar(a.src, b.src)
        }
    }

    def derivScalarVarWrtRight(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(sc(r1), varGen(r2)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).deriv(b.node) shouldBe d.derivScalarVarWrtRight(a.src, b.src)
        }
    }

    def derivScalarVarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(sc(r1), varGen(r2), varGen(r3)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T], c: VarSample[U, T]) =>
            d.op(a.node, b.node).deriv(c.node) shouldBe d.derivScalarVarWrtUnknown(a.src, b.src)
        }
    }

    def derivContainerVarWrtRight(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(cc(r2), varGen(r2)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).deriv(b.node) shouldBe d.derivContainerVarWrtRight(a.src, b.src)
        }
    }

    def derivContainerVarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(cc(r2), varGen(r2), varGen(r3)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T], c: VarSample[U, T]) =>
            d.op(a.node, b.node).deriv(c.node) shouldBe d.derivContainerVarWrtUnknown(a.src, b.src)
        }
    }

    def derivVarScalarWrtLeft(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), sc(r2)) { case (a: VarSample[U, T], b: ScalarConstSample[U, T]) =>
            d.op(a.node, b.node).deriv(a.node) shouldBe d.derivVarScalarWrtLeft(a.src, b.src)
        }
    }

    def derivVarScalarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(varGen(r1), sc(r2), varGen(r3)) { case (a: VarSample[U, T], b: ScalarConstSample[U, T], c: VarSample[U, T]) =>
            d.op(a.node, b.node).deriv(c.node) shouldBe d.derivVarScalarWrtUnknown(a.src, b.src)
        }
    }

    def derivVarContainerWrtLeft(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), cc(r2)) { case (a: VarSample[U, T], b: ContainerConstSample[U, T]) =>
            d.op(a.node, b.node).deriv(a.node) shouldBe d.derivVarContainerWrtLeft(a.src, b.src)
        }
    }

    def derivVarContainerWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(varGen(r1), cc(r2), varGen(r3)) { case (a: VarSample[U, T], b: ContainerConstSample[U, T], c: VarSample[U, T]) =>
            d.op(a.node, b.node).deriv(c.node) shouldBe d.derivVarContainerWrtUnknown(a.src, b.src)
        }
    }

    def derivVarVarWrtLeft(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), varGen(r2)) { case (a: VarSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).deriv(a.node) shouldBe d.derivVarVarWrtLeft(a.src, b.src)
        }
    }

    def derivVarVarWrtRight(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), varGen(r2)) { case (a: VarSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).deriv(b.node) shouldBe d.derivVarVarWrtRight(a.src, b.src)
        }
    }

    def derivVarVarWrtUnknown(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(varGen(r1), varGen(r2), varGen(r3)) { case (a: VarSample[U, T], b: VarSample[U, T], c: VarSample[U, T]) =>
            d.op(a.node, b.node).deriv(c.node) shouldBe d.derivVarVarWrtUnknown(a.src, b.src)
        }
    }

    def derivVarVarWrtSelf(r: Restriction = default): Prop = {
        forAll(varGen(r)) { case a: VarSample[U, T] =>
            d.op(a.node, a.node).deriv(a.node) shouldBe d.derivVarVarWrtSelf(a.src)
        }
    }

    def propagateScalarScalarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(sc(r1), sc(r2), ncv(r3)) { case (a: ScalarConstSample[U, T], b: ScalarConstSample[U, T], c: NonContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarScalarWithNCValue(a.src, b.src, c.src)
        }
    }

    def propagateScalarContainerWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(sc(r1), cc(r2), ncv(r3)) { case (a: ScalarConstSample[U, T], b: ContainerConstSample[U, T], c: NonContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarContainerWithNCValue(a.src, b.src, c.src)
        }
    }

    def propagateScalarVarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(sc(r1), varGen(r2), ncv(r3)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T], c: NonContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarVarWithNCValue(a.src, b.src, c.src)
        }
    }

    def propagateContainerScalarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(cc(r1), sc(r2), ncv(r3)) { case (a: ContainerConstSample[U, T], b: ScalarConstSample[U, T], c: NonContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerScalarWithNCValue(a.src, b.src, c.src)
        }
    }

    def propagateContainerContainerWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(cc(r1), cc(r2), ncv(r3)) { case (a: ContainerConstSample[U, T], b: ContainerConstSample[U, T], c: NonContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerContainerWithNCValue(a.src, b.src, c.src)
        }
    }

    def propagateContainerVarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(cc(r1), varGen(r2), ncv(r3)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T], c: NonContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerVarWithNCValue(a.src, b.src, c.src)
        }
    }

    private[this] def cc(r: Restriction) = nodes.genContainerConstWithSource(r)

    private[this] def ncv(r: Restriction) = values.genNonContainerValueWithSource(r)

    def propagateVarScalarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(varGen(r1), sc(r2), ncv(r3)) { case (a: VarSample[U, T], b: ScalarConstSample[U, T], c: NonContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarScalarWithNCValue(a.src, b.src, c.src)
        }
    }

    def propagateVarContainerWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(varGen(r1), cc(r2), ncv(r3)) { case (a: VarSample[U, T], b: ContainerConstSample[U, T], c: NonContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarContainerWithNCValue(a.src, b.src, c.src)
        }
    }

    def propagateVarVarWithNCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(varGen(r1), varGen(r2), ncv(r3)) { case (a: VarSample[U, T], b: VarSample[U, T], c: NonContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarVarWithNCValue(a.src, b.src, c.src)
        }
    }

    def propagateScalarScalarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(sc(r1), sc(r2), cv(r3)) { case (a: ScalarConstSample[U, T], b: ScalarConstSample[U, T], c: ContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarScalarWithCValue(a.src, b.src, c.src)
        }
    }

    def propagateScalarContainerWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(sc(r1), cc(r2), cv(r3)) { case (a: ScalarConstSample[U, T], b: ContainerConstSample[U, T], c: ContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarContainerWithCValue(a.src, b.src, c.src)
        }
    }

    def propagateScalarVarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(sc(r1), varGen(r2), cv(r3)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T], c: ContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateScalarVarWithCValue(a.src, b.src, c.src)
        }
    }

    private[this] def sc(r: Restriction) = nodes.genScalarConstWithSource(r)

    private[this] def varGen(r: Restriction) = nodes.genVarWithSource(r)

    private[this] def cv(r: Restriction) = values.genContainerValueWithSource(r)

    def propagateContainerScalarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(cc(r1), sc(r2), cv(r3)) { case (a: ContainerConstSample[U, T], b: ScalarConstSample[U, T], c: ContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerScalarWithCValue(a.src, b.src, c.src)
        }
    }

    def propagateContainerContainerWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(cc(r1), cc(r2), cv(r3)) { case (a: ContainerConstSample[U, T], b: ContainerConstSample[U, T], c: ContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerContainerWithCValue(a.src, b.src, c.src)
        }
    }

    def propagateContainerVarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(cc(r1), varGen(r2), cv(r3)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T], c: ContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateContainerVarWithCValue(a.src, b.src, c.src)
        }
    }

    def propagateVarScalarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(varGen(r1), sc(r2), cv(r3)) { case (a: VarSample[U, T], b: ScalarConstSample[U, T], c: ContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarScalarWithCValue(a.src, b.src, c.src)
        }
    }

    def propagateVarContainerWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(varGen(r1), cc(r2), cv(r3)) { case (a: VarSample[U, T], b: ContainerConstSample[U, T], c: ContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarContainerWithCValue(a.src, b.src, c.src)
        }
    }

    def propagateVarVarWithCValue(r1: Restriction = default, r2: Restriction = default, r3: Restriction = default): Prop = {
        forAll(varGen(r1), varGen(r2), cv(r3)) { case (a: VarSample[U, T], b: VarSample[U, T], c: ContainerValueSample[U, T]) =>
            d.op(a.node, b.node).propagate(c.value) shouldBe d.propagateVarVarWithCValue(a.src, b.src, c.src)
        }
    }

    def gradScalarScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(sc(r1), sc(r2)) { case (a: ScalarConstSample[U, T], b: ScalarConstSample[U, T]) =>
            d.op(a.node, b.node).grad() shouldBe d.gradScalarScalar(a.src, b.src)
        }
    }

    def gradScalarContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(sc(r1), cc(r2)) { case (a: ScalarConstSample[U, T], b: ContainerConstSample[U, T]) =>
            d.op(a.node, b.node).grad() shouldBe d.gradScalarContainer(a.src, b.src)
        }
    }

    def gradScalarVar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(sc(r1), varGen(r2)) { case (a: ScalarConstSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).grad() shouldBe d.gradScalarVar(a.src, b.src)
        }
    }

    def gradContainerScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(cc(r1), sc(r2)) { case (a: ContainerConstSample[U, T], b: ScalarConstSample[U, T]) =>
            d.op(a.node, b.node).grad() shouldBe d.gradContainerScalar(a.src, b.src)
        }
    }

    def gradContainerContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(cc(r1), cc(r2)) { case (a: ContainerConstSample[U, T], b: ContainerConstSample[U, T]) =>
            d.op(a.node, b.node).grad() shouldBe d.gradContainerContainer(a.src, b.src)
        }
    }

    def gradContainerVar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(cc(r1), varGen(r2)) { case (a: ContainerConstSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).grad() shouldBe d.gradContainerVar(a.src, b.src)
        }
    }

    def gradVarScalar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), sc(r2)) { case (a: VarSample[U, T], b: ScalarConstSample[U, T]) =>
            d.op(a.node, b.node).grad() shouldBe d.gradVarScalar(a.src, b.src)
        }
    }

    def gradVarContainer(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), cc(r2)) { case (a: VarSample[U, T], b: ContainerConstSample[U, T]) =>
            d.op(a.node, b.node).grad() shouldBe d.gradVarContainer(a.src, b.src)
        }
    }

    def gradVarVar(r1: Restriction = default, r2: Restriction = default): Prop = {
        forAll(varGen(r1), varGen(r2)) { case (a: VarSample[U, T], b: VarSample[U, T]) =>
            d.op(a.node, b.node).grad() shouldBe d.gradVarVar(a.src, b.src)
        }
    }

    private[this] def node(r: Restriction) = nodes.genNode(r)

    private[this] def value(r: Restriction) = values.genValue(r)

}
