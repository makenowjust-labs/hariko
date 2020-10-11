package codes.quine.labo.hariko.random

import SplitMix64.{mix64, mixGamma}

/** SplitMix64 is an immutable 64-bit SplitMix implementation.
  *
  * SplitMix is one of splittable pseudo-random number generators (PRNG).
  * It has useful features for Property Based Testing.
  *
  * Reference:
  *
  * - Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. (2014) "Fast splittable pseudorandom number generators."
  *   [[https://doi.org/10.1145/2660193.2660195]]
  */
final case class SplitMix64 private (x: Long, gamma: Long) {

  /** Returns a current random value.
    */
  def value: Long = mix64(x + gamma)

  /** Returns a next PRNG.
    */
  def next: SplitMix64 = new SplitMix64(x + gamma, gamma)

  /** Splits this into two variants.
    */
  def split: (SplitMix64, SplitMix64) =
    (new SplitMix64(x + gamma * 2, gamma), new SplitMix64(mix64(x + gamma), mixGamma(x + gamma * 2)))

  /** Returns left variant. It is the same as `split._1`.
    */
  def left: SplitMix64 = new SplitMix64(x + gamma * 2, gamma)

  /** Returns right variant. It is the same as `split._2`.
    */
  def right: SplitMix64 = new SplitMix64(mix64(x + gamma), mixGamma(x + gamma * 2))
}

object SplitMix64 {
  private val GoldenGamma: Long = 0x9e3779b97f4a7c15L

  /** Creates a new PRNG instance from the seed.
    */
  def apply(x: Long): SplitMix64 = SplitMix64(mix64(x), mixGamma(GoldenGamma + x))

  /** Creates a new [[SplitMix64]] instance from the seed and `gamma`.
    */
  def apply(x: Long, gamma: Long): SplitMix64 = new SplitMix64(x, gamma | 1)

  private def mix64(z0: Long): Long = {
    val z1 = (z0 ^ (z0 >>> 33)) * 0xff51afd7ed558ccdL
    val z2 = (z1 ^ (z1 >>> 33)) * 0xc4ceb9fe1a85ec53L
    z2 ^ (z2 >>> 33)
  }

  private def mixGamma(z0: Long): Long = {
    val z1 = (z0 ^ (z0 >>> 30)) * 0xbf58476d1ce4e5b9L
    val z2 = (z1 ^ (z1 >>> 27)) * 0x94d049bb133111ebL
    val z3 = (z2 ^ (z2 >>> 31)) | 1
    val n = java.lang.Long.bitCount(z3 ^ (z3 >>> 1))
    if (n >= 24) z3 else z3 ^ 0xaaaaaaaaaaaaaaaaL
  }
}
