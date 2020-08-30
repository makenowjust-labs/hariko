package codes.quine.labo.gen.random

/**
  * SplitMix64 is an immutable splitmix64 implementation.
  *
  * @see [[http://xoroshiro.di.unimi.it/splitmix64.c]] - C reference implementation
  */
final case class SplitMix64(x: Long) {
  /** Returns a current random value. */
  def value: Long = {
    var z = x + 0x9e3779b97f4a7c15L
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)
  }

  /** Returns a next RNG. */
  def next: SplitMix64 = new SplitMix64(x + 0x9e3779b97f4a7c15L)
}
