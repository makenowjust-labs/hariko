package codes.quine.labo.gen.data

final class LocalFun[T, R](x: T, r: R) extends (T => R) {
  def apply(x0: T): R = if (x0 == x) r else sys.error("gen.data.PairFun: unknown value is given")

  override def toString: String = s"{ case $x => $r; case _ => ??? }"
}
