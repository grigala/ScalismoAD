package ch.grigala.scalismoad.rule

// for interactions between (DenseMatrix[x], DenseVector[x], x=Double)
trait ValueRuleBreeze[U[_], V[_], T] extends ValueRule[U, T] {

    def mulMV(l: U[T], r: V[T]): V[T]

}
