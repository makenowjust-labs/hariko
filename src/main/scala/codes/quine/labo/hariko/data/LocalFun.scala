package codes.quine.labo.hariko
package data

import util.Show

/**
  * LocalFun is a function defined in `x` locally.
  */
final class LocalFun[T, R](x: T, y: R) extends (T => R) {

  /**
    * Calls this function with argument.
    *
    * If argument does not equals to `x`, then it is not defined so it throws `RuntimeException`.
    */
  def apply(x0: T): R = if (x0 == x) y else sys.error("hariko.data.LocalFun: undefined")

  override def toString: String = s"{case ${Show.any(x)} => ${Show.any(y)}}"
}
