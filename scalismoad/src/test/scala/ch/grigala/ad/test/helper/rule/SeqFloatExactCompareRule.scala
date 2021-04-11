package ch.grigala.ad.test.helper.rule

class SeqFloatExactCompareRule extends CompareRule[Seq, Float] {

    def shouldBe(a: Seq[Float], b: Seq[Float])(implicit d: DummyImplicit): Boolean = {
        a.zip(b).map({ case (x, y) =>
            x == y || (x.equals(Float.NaN) && y.equals(Float.NaN))
        }).forall(b => b)
    }

    def shouldBe(a: Float, b: Float): Boolean = {
        a == b || (a.equals(Float.NaN) && b.equals(Float.NaN))
    }

}
