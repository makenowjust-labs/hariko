package codes.quine.labo.hariko.data

/**
  * DataOps provides extensions of `data` package types.
  */
object DataOps {

  /**
    * RangeOps provides extension methods of `Range` type.
    */
  implicit final class RangeOps[T](private val r1: Range[T]) extends AnyVal {

    /**
      * Checks equality between `r1` and `r2`.
      */
    def ===(r2: Range[T])(implicit T: Ordering[T]): Boolean =
      (0 to 100).forall(k => r1.bounds(k) == r2.bounds(k))
  }
}
