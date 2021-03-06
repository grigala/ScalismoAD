package ch.grigala.scalismoad

object Utils {

    def ~=(x: Double, y: Double, precision: Double = 1e-5): Unit = {
        assert((x - y).abs < precision, s"expected $y, got $x, precision=$precision")
    }
}
