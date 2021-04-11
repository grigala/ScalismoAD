package ch.grigala.ad.test.helper.rule

class SeqFloatSoftCompareRule extends CompareRule[Seq, Float] {

    val eps = (x: Float) => scala.math.abs(x) * 1e-3f

    def shouldBe(a: Seq[Float], b: Seq[Float])(implicit d: DummyImplicit): Boolean = {
        a.zip(b).map({ case (x, y) =>
            x == y || scala.math.abs(x - y) <= eps(x) || (x.equals(Float.NaN) && y.equals(Float.NaN))
        }).forall(b => b)
    }

    def shouldBe(a: Float, b: Float): Boolean = {
        a == b || scala.math.abs(a - b) <= eps(a) || (a.equals(Float.NaN) && b.equals(Float.NaN))
    }

}
