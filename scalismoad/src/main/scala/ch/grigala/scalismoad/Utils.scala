package ch.grigala.scalismoad

object Utils {

    def ~=(x: Double, y: Double, precision: Double): Boolean = {
        if ((x - y).abs < precision) {
            true
        } else {
            println(s"expected $y, got $x, precision=$precision")
            false
        }
    }
}
