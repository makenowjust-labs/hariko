package codes.quine.labo.hariko
package data

import PartialFun.:=>
import util.Show

/**
  * Fun is data structure represents a function.
  *
  * It is showable and shrinkable. It alsp inherits
  * `Function1` type, so we can use it like as function.
  */
final case class Fun[T, R](pfun: T :=> Option[R], fallback: R) extends (T => R) {

  /**
    * Calls this function with argument.
    */
  def apply(x: T): R = pfun.lift(x).flatten.getOrElse(fallback)

  /**
    * Shows this function content.
    *
    * It is potentially infinite result, so we can restrict
    * maximum argument-result table size by `maxTableSize`.
    */
  def toString(maxTableSize: Int): String = {
    val table = pfun.table.collect { case (a, y) => (a, y.getOrElse(fallback)) }
    val hasRemains = table.lengthIs > maxTableSize
    val map = table.take(maxTableSize).groupMap(_._2)(_._1)
    val list =
      map.toList.map {
        case (y, xs) => s"case ${xs.map(Show.any).mkString(" | ")} => ${Show.any(y)}"
      } ++
        (if (hasRemains) List("... ") else List.empty) ++
        List(s"case _ => ${Show.any(fallback)}")
    list.mkString("{", "; ", "}")
  }

  override def toString(): String = toString(16)
}
