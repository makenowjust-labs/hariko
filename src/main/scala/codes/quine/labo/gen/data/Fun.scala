package codes.quine.labo.gen.data

import PartialFun.:=>

final case class Fun[T, R](pfun: T :=> Option[R], fallback: R, isShrunk: Boolean) extends (T => R) {
  def apply(x: T): R = pfun.lift(x).flatten.getOrElse(fallback)

  override def toString: String =
    if (isShrunk) {
      val table = pfun.table.collect { case (a, Some(b)) => s"case $a => $b" } :+ s"case _ => $fallback"
      table.mkString("{ ", "; ", " }")
    } else s"Fun($pfun, $fallback, $isShrunk)"
}
