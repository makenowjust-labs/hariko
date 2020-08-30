package codes.quine.labo.gen.random

/** Random is [[Xoroshiro128]] wrapper. */
final case class Random(xoroshiro: Xoroshiro128) {
  /**
    * Generates a random Long value.
    *
    * Long value range is `Long.MinValue <= x <= Long.MinValue`.
    */
  def nextLong: (Random, Long) =
    (Random(xoroshiro.next), xoroshiro.value)

  /**
    * Generates a random Long value in bounds.
    *
    * Long value range is `min <= x <= max`, where `(min, max) = bounds`.
    */
  def nextLong(bounds: (Long, Long)): (Random, Long) = {
    val (min, max) = bounds
    require(min <= max, "gen.random.Random#nextLong: invalid bounds")
    if (min == max) (this, min)
    else if (min == Long.MinValue && max == Long.MaxValue) nextLong
    else {
      val (rand, x) = nextLong
      if (max - min > 0 && max - min + 1 > 0) (rand, min + (x >>> 1) % (max - min + 1))
      // TODO: better overflow handling
      else (rand, min + ((BigInt(x) - Long.MinValue) % (BigInt(max) - min + 1)).toLong)
    }
  }

  /**
    * Generates a random Double value.
    *
    * Double value range is `0 <= x <= 1`.
    */
  def nextDouble: (Random, Double) = {
    val (rand, x) = nextLong
    (rand, (x >>> 11).toDouble / ((1L << 53) - 1).toDouble)
  }

  /**
    * Generates a random Double value in bounds.
    *
    * Double value range is `min <= x <= max`, where `(min, max) = bounds`.
    */
  def nextDouble(bounds: (Double, Double)): (Random, Double) = {
    val (min, max) = bounds
    require(min <= max && !min.isNaN && min.isFinite && !max.isNaN && max.isFinite, "gen.random.Random#nextDouble: invalid bounds")
    val (rand, x) = nextDouble
    (rand, 2.0 * (min * 0.5 + x % (max * 0.5 - min * 0.5)))
  }
}

object Random {
  /** Creates a new [[Random]] instance with the given seed. */
  def apply(seed: Long): Random = Random(Xoroshiro128(seed))
}
