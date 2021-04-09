package ch.grigala.ad.graph


// Special single value container for handling non-collection variables.
case class Scalar[T](val data: T)
