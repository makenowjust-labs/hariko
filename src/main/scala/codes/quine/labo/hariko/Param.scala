package codes.quine.labo.hariko

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

import random.Random

/**
  * Param is a parameter of property execution.
  *
  * @param seed a seed of PRNG for testing
  * @param minSuccessful what number of running a property
  * @param maxDiscarded what number to accept discarded value
  * @param minScale minimal scale value
  * @param maxScale maximal scale value
  * @param maxShrink what number of trying to shrink a counter example
  * @param timeout a timeout duration for testing
  */
final case class Param(
    seed: Long,
    minSuccessful: Int = 100,
    maxDiscarded: Int = 16,
    minScale: Int = 0,
    maxScale: Int = 100,
    maxShrink: Int = 50000,
    timeout: Duration = Duration(5, TimeUnit.SECONDS)
) {

  /**
    * Builds a PRNG from this seed.
    */
  def toRandom: Random = Random(seed)
}
