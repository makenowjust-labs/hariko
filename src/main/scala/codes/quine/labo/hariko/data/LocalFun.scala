package codes.quine.labo.hariko
package data

import util.Show

final class LocalFun[T, R](x: T, r: R) extends (T => R) {
  def apply(x0: T): R = if (x0 == x) r else sys.error("hariko.data.PairFun: unknown value is given")

  override def toString: String = s"{ case ${Show.any(x)} => ${Show.any(r)}; case _ => ??? }"
}
