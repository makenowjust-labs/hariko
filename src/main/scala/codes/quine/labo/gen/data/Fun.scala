package codes.quine.labo.gen
package data

import PartialFun.:=>
import util.Show

final case class Fun[T, R](pfun: T :=> Option[R], fallback: R, isShrunk: Boolean) extends (T => R) {
  def apply(x: T): R = pfun.lift(x).flatten.getOrElse(fallback)

  override def toString: String =
    if (isShrunk) {
      val table = pfun.table.collect {
        case (a, Some(b)) => s"case ${Show.any(a)} => ${Show.any(b)}"
      } :+ s"case _ => ${Show.any(fallback)}"
      table.mkString("{ ", "; ", " }")
    } else s"Fun($pfun, $fallback, $isShrunk)"
}
