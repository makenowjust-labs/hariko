package codes.quine.labo.hariko

/**
  * HarikoOps provides extensions of `hariko` package types.
  */
object HarikoOps {

  /**
    * GenOps provides extension methods of `Gen` type.
    */
  implicit final class GenOps[T](private val gen1: Gen[T]) extends AnyVal {

    /**
      * Checks equality between `gen1` and `gen`.
      *
      * At least, `gen1` does not equel to `gen2` when it returns `false`.
      */
    def ===(gen2: Gen[T]): Boolean =
      gen1.samples().take(100) == gen2.samples().take(100)
  }
}
