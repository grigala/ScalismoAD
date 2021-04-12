package ch.grigala.scalismoad.graph


// Special single value container for handling non-collection variables.
case class Scalar[T](val data: T) {
    def getData: T = data
}
