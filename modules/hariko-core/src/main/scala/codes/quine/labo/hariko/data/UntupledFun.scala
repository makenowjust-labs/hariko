package codes.quine.labo.hariko.data

/** Like `Function.untupled`, but it keeps string representation.
  */
object UntupledFun {

  /** A two elements tupled function wrapper for keeping stirng representation.
    */
  final class UntupledFun2[T1, T2, R](val fun: ((T1, T2)) => R) extends ((T1, T2) => R) {

    /** Calls this function.
      */
    def apply(x1: T1, x2: T2): R = fun(x1, x2)

    override def toString: String = fun.toString
  }

  /** A three elements tupled function wrapper for keeping stirng representation.
    */
  final class UntupledFun3[T1, T2, T3, R](val fun: ((T1, T2, T3)) => R) extends ((T1, T2, T3) => R) {

    /** Calls this function.
      */
    def apply(x1: T1, x2: T2, x3: T3): R = fun(x1, x2, x3)

    override def toString: String = fun.toString
  }

  /** A four elements tupled function wrapper for keeping stirng representation.
    */
  final class UntupledFun4[T1, T2, T3, T4, R](val fun: ((T1, T2, T3, T4)) => R) extends ((T1, T2, T3, T4) => R) {

    /** Calls this function.
      */
    def apply(x1: T1, x2: T2, x3: T3, x4: T4): R = fun(x1, x2, x3, x4)

    override def toString: String = fun.toString
  }
}
