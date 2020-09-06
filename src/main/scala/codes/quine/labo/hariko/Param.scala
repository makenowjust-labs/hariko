package codes.quine.labo.hariko

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

import random.Random

final case class Param(
    seed: Int,
    minSuccessful: Int = 100,
    maxDiscarded: Int = 16,
    minScale: Int = 0,
    maxScale: Int = 100,
    maxShrink: Int = 10000,
    timeout: Duration = Duration(5, TimeUnit.SECONDS)
) {
  def toRandom: Random = Random(seed)
}
