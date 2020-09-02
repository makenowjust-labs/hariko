package codes.quine.labo.gen.data

import PartialFun.:=>

final case class Fun[T, R](pfun: T :=> Option[Tree[R]], fallback: R, shrunk: Boolean) extends (T => R) {
  def apply(x: T): R = pfun.toFunction(None)(x).map(_.value).getOrElse(fallback)

  override def toString: String =
    if (shrunk) {
      val table = pfun.table.collect { case (a, Some(Tree(b, _))) => s"$a -> $b" }.mkString(", ")
      s"Fun.from($table)($fallback)"
    } else s"Fun($pfun, $fallback, $shrunk)"
}
