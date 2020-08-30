package codes.quine.labo.gen.random

/**
  * Xoroshiro128 is an immutable xoroshiro128+ implementation.
  *
  * @see [[http://prng.di.unimi.it/xoroshiro128plus.c]] - C reference implementation
  */
final case class Xoroshiro128(a: Long, b: Long) {
  @inline private[this] def rotl(x: Long, k: Int): Long = x << k | (x >>> (64 - k))

  /** Returns a current random value. */
  def value: Long = a + b

  /** Returns a next RNG. */
  def next: Xoroshiro128 = {
    val x = a ^ b
    Xoroshiro128(rotl(a, 24) ^ x ^ (x << 16), rotl(x, 37))
  }
}

object Xoroshiro128 {
  /**
    * Creates a new [[Xoroshiro128]] instance with the given `seed`.
    *
    * A `seed` is extended to 128bit by using [[SplitMix64]].
    */
  def apply(seed: Long): Xoroshiro128 = {
    val splitmix0 = new SplitMix64(seed)
    val a = splitmix0.value
    val splitmix1 = splitmix0.next
    val b = splitmix1.value
    Xoroshiro128(a, b)
  }
}
