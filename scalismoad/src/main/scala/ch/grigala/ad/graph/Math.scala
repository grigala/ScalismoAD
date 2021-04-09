package ch.grigala.ad.graph

import ch.grigala.ad.rule.MathRule
import ch.grigala.ad.value.{ContainerValue, NonContainerValue, Value}

class sin[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"sin(${v})"

    override def apply(): Value[U, T] = sin(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = cos(v()) * v.deriv(wrt)

    override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(g * cos(v()))
}

class cos[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"cos(${v})"

    override def apply(): Value[U, T] = cos(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = -sin(v()) * v.deriv(wrt)

    override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(-sin(v()) * g)
}

class tan[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"tan(${v})"

    override def apply(): Value[U, T] = tan(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = {
        val tan_v_val = tan(v())
        v.deriv(wrt) * (vr.one + tan_v_val * tan_v_val)
    }

    override def propagate(g: Value[U, T]): Value[U, T] = {
        val tan_v_val = tan(v())
        v.propagate(g * (vr.one + tan_v_val * tan_v_val))
    }
}

class asin[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"asin(${v})"

    override def apply(): Value[U, T] = asin(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = {
        val v_val = v()
        val one = vr.one
        val d = one / sqrt(one - (v_val * v_val))
        d * v.deriv(wrt)
    }

    override def propagate(g: Value[U, T]): Value[U, T] = {
        val v_val = v()
        val one = vr.one
        val d = one / sqrt(one - (v_val * v_val))
        v.propagate(g * d)
    }
}

class acos[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"acos(${v})"

    override def apply(): Value[U, T] = acos(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = {
        val v_val = v()
        val one = vr.one
        val d = -(one / sqrt(one - (v_val * v_val)))
        d * v.deriv(wrt)
    }

    override def propagate(g: Value[U, T]): Value[U, T] = {
        val v_val = v()
        val one = vr.one
        val d = -(one / sqrt(one - (v_val * v_val)))
        v.propagate(g * d)
    }
}

class atan[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"atan(${v})"

    override def apply(): Value[U, T] = atan(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = {
        val v_val = v()
        val one = vr.one
        val d = one / (one + (v_val * v_val))
        d * v.deriv(wrt)
    }

    override def propagate(g: Value[U, T]): Value[U, T] = {
        val v_val = v()
        val one = vr.one
        val d = one / (one + (v_val * v_val))
        v.propagate(g * d)
    }
}

class sinh[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"sinh(${v})"

    override def apply(): Value[U, T] = sinh(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = cosh(v()) * v.deriv(wrt)

    override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(g * cosh(v()))
}

class cosh[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"cosh(${v})"

    override def apply(): Value[U, T] = cosh(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = sinh(v()) * v.deriv(wrt)

    override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(sinh(v()) * g)
}

class tanh[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"tanh(${v})"

    override def apply(): Value[U, T] = tanh(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = {
        val tanh_v_val = tanh(v())
        val one = vr.one
        v.deriv(wrt) * (one - tanh_v_val * tanh_v_val)
    }

    override def propagate(g: Value[U, T]): Value[U, T] = {
        val tanh_v_val = tanh(v())
        val one = vr.one
        v.propagate(g * (one - tanh_v_val * tanh_v_val))
    }
}

class ln[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"ln(${v})"

    override def apply(): Value[U, T] = ln(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = v.deriv(wrt) / v()

    override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(g / v())
}

class exp[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"exp(${v})"

    override def apply(): Value[U, T] = exp(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = v.deriv(wrt) * exp(v())

    override def propagate(g: Value[U, T]): Value[U, T] = v.propagate(g * exp(v()))
}

class abs[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"|${v}|"

    override def apply(): Value[U, T] = where(v() >= r.zero, v(), -v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = where(v() >= r.zero, v.deriv(wrt), -v.deriv(wrt))

    override def propagate(g: Value[U, T]): Value[U, T] = where(v() >= r.zero, v.propagate(g), v.propagate(-g))
}

// this may be unnecessary because it can be replaced with pow(x, 1/2)
class sqrt[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp[U, T] {
    override def toString: String = s"sqrt(${v})"

    override def apply(): Value[U, T] = sqrt(v())

    override def deriv(wrt: Var[U, T]): Value[U, T] = {
        val half = r.one / (r.one + r.one)
        val minus_half = half - r.one
        v.deriv(wrt) * half * pow(v(), minus_half)
    }

    override def propagate(g: Value[U, T]): Value[U, T] = {
        val half = r.one / (r.one + r.one)
        val minus_half = half - r.one
        val gv = half * pow(v(), minus_half)
        v.propagate(g * gv)
    }
}

class pow[U[_], T](a: Node[U, T], b: Node[U, T])(implicit r: MathRule[U, T]) extends BinaryOp[U, T] {
    override def toString: String = s"pow(${a}, ${b} )"

    override def apply(): Value[U, T] = pow(a(), b())

    override def deriv(wrt: Var[U, T]): Value[U, T] = {
        val a_val = a()
        val b_val = b()
        val b_minus_one = b_val - r.one

        val lhs = a.deriv(wrt) * b_val * pow(a_val, b_minus_one)
        val rhs = b.deriv(wrt) * ln(a_val) * pow(a_val, b_val)
        lhs + rhs
    }

    override def propagate(g: Value[U, T]): Value[U, T] = {
        val a_val = a()
        val b_val = b()
        val b_minus_one = b_val - r.one

        val lhs = a.propagate(g * b_val * pow(a_val, b_minus_one))
        val rhs = b.propagate(g * ln(a_val) * pow(a_val, b_val))
        lhs + rhs
    }
}

// mimic the behavior of numpy's maximum
// http://docs.scipy.org/doc/numpy/reference/generated/numpy.maximum.html
class max[U[_], T](a: Node[U, T], b: Node[U, T])(implicit r: MathRule[U, T]) extends BinaryOp[U, T] {
    override def toString: String = s"max(${a}, ${b})"

    override def apply(): Value[U, T] = where(a() >= b(), a(), b())

    override def deriv(wrt: Var[U, T]): Value[U, T] = where(a() >= b(), a.deriv(wrt), b.deriv(wrt))

    override def propagate(g: Value[U, T]): Value[U, T] = where(a() >= b(), a.propagate(g), b.propagate(g))
}

// mimic the behavior of numpy's minimum
// http://docs.scipy.org/doc/numpy/reference/generated/numpy.minimum.html
class min[U[_], T](a: Node[U, T], b: Node[U, T])(implicit r: MathRule[U, T]) extends BinaryOp[U, T] {
    override def toString: String = s"min(${a}, ${b}})"

    override def apply(): Value[U, T] = where(a() <= b(), a(), b())

    override def deriv(wrt: Var[U, T]): Value[U, T] = where(a() <= b(), a.deriv(wrt), b.deriv(wrt))

    override def propagate(g: Value[U, T]): Value[U, T] = where(a() <= b(), a.propagate(g), b.propagate(g))
}

// This have the same behavior as numpy's dot function
// http://docs.scipy.org/doc/numpy/reference/generated/numpy.dot.html
class dot[U[_], T](a: Node[U, T], b: Node[U, T])(implicit r: MathRule[U, T]) extends BinaryOp[U, T] {
    override def toString: String = s"${a}・${b})"

    override def apply(): Value[U, T] = dot(a(), b())

    override def deriv(wrt: Var[U, T]): Value[U, T] = dot(a.deriv(wrt), b().T) + dot(a().T, b.deriv(wrt))

    override def propagate(g: Value[U, T]): Value[U, T] = a.propagate(dot(g, b().T)) + b.propagate(dot(a().T, g))
}


object sin {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): sin[U, T] = new sin(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.sinM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.sinS(v.data))
    }
}

object cos {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): cos[U, T] = new cos(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.cosM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.cosS(v.data))
    }
}

object tan {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): tan[U, T] = new tan(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.tanM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.tanS(v.data))
    }
}

object asin {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): asin[U, T] = new asin(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.asinM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.asinS(v.data))
    }
}

object acos {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): acos[U, T] = new acos(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.acosM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.acosS(v.data))
    }
}

object atan {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): atan[U, T] = new atan(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.atanM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.atanS(v.data))
    }
}

object sinh {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): sinh[U, T] = new sinh(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.sinhM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.sinhS(v.data))
    }
}

object cosh {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): cosh[U, T] = new cosh(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.coshM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.coshS(v.data))
    }
}

object tanh {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): tanh[U, T] = new tanh(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.tanhM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.tanhS(v.data))
    }
}

object ln {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): ln[U, T] = new ln(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.lnM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.lnS(v.data))
    }
}

object exp {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): exp[U, T] = new exp(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.expM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.expS(v.data))
    }
}

object abs {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): abs[U, T] = new abs(v)
}

object sqrt {
    def apply[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]): sqrt[U, T] = new sqrt(v)

    def apply[U[_], T](v: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = v match {
        case v: NonContainerValue[U, T] => NonContainerValue[U, T](mr.sqrtM(v.data))
        case v: ContainerValue[U, T] => ContainerValue[U, T](mr.sqrtS(v.data))
    }
}

object pow {
    def apply[U[_], T](v: Node[U, T], p: Node[U, T])(implicit vr: MathRule[U, T]): pow[U, T] = new pow(v, p)

    def apply[U[_], T](v: Value[U, T], p: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = (v, p) match {
        case (v: NonContainerValue[U, T], p: NonContainerValue[U, T]) => NonContainerValue[U, T](mr.powMM(v.data, p.data))
        case (v: NonContainerValue[U, T], p: ContainerValue[U, T]) => ContainerValue[U, T](mr.powMS(v.data, p.data))
        case (v: ContainerValue[U, T], p: NonContainerValue[U, T]) => ContainerValue[U, T](mr.powSM(v.data, p.data))
        case (v: ContainerValue[U, T], p: ContainerValue[U, T]) => ContainerValue[U, T](mr.powSS(v.data, p.data))
    }
}

object max {
    def apply[U[_], T](a: Node[U, T], b: Node[U, T])(implicit vr: MathRule[U, T]): max[U, T] = new max(a, b)
}

object min {
    def apply[U[_], T](a: Node[U, T], b: Node[U, T])(implicit vr: MathRule[U, T]): min[U, T] = new min(a, b)
}

object dot {
    def apply[U[_], T](a: Node[U, T], b: Node[U, T])(implicit vr: MathRule[U, T]): dot[U, T] = new dot(a, b)

    def apply[U[_], T](a: Value[U, T], b: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = (a, b) match {
        case (a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => NonContainerValue[U, T](mr.dotMM(a.data, b.data))
        case (a: NonContainerValue[U, T], b: ContainerValue[U, T]) => ContainerValue[U, T](mr.dotMS(a.data, b.data))
        case (a: ContainerValue[U, T], b: NonContainerValue[U, T]) => ContainerValue[U, T](mr.dotSM(a.data, b.data))
        case (a: ContainerValue[U, T], b: ContainerValue[U, T]) => ContainerValue[U, T](mr.dotSS(a.data, b.data))
    }
}

// There is no Node which represents this functionality
object where {
    def apply[U[_], T](cond: Value[U, Boolean], a: Value[U, T], b: Value[U, T])(implicit mr: MathRule[U, T]): Value[U, T] = (cond, a, b) match {
        case (cond: NonContainerValue[U, Boolean], a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => NonContainerValue[U, T](mr.whereMMM(cond.data, a.data, b.data))
        case (cond: NonContainerValue[U, Boolean], a: NonContainerValue[U, T], b: ContainerValue[U, T]) => ContainerValue[U, T](mr.whereMMS(cond.data, a.data, b.data))
        case (cond: NonContainerValue[U, Boolean], a: ContainerValue[U, T], b: NonContainerValue[U, T]) => ContainerValue[U, T](mr.whereMSM(cond.data, a.data, b.data))
        case (cond: NonContainerValue[U, Boolean], a: ContainerValue[U, T], b: ContainerValue[U, T]) => ContainerValue[U, T](mr.whereMSS(cond.data, a.data, b.data))
        case (cond: ContainerValue[U, Boolean], a: NonContainerValue[U, T], b: NonContainerValue[U, T]) => ContainerValue[U, T](mr.whereSMM(cond.data, a.data, b.data))
        case (cond: ContainerValue[U, Boolean], a: NonContainerValue[U, T], b: ContainerValue[U, T]) => ContainerValue[U, T](mr.whereSMS(cond.data, a.data, b.data))
        case (cond: ContainerValue[U, Boolean], a: ContainerValue[U, T], b: NonContainerValue[U, T]) => ContainerValue[U, T](mr.whereSSM(cond.data, a.data, b.data))
        case (cond: ContainerValue[U, Boolean], a: ContainerValue[U, T], b: ContainerValue[U, T]) => ContainerValue[U, T](mr.whereSSS(cond.data, a.data, b.data))
    }
}

