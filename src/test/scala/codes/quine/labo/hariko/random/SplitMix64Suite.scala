package codes.quine.labo.hariko.random

import minitest.SimpleTestSuite

object SplitMix64Suite extends SimpleTestSuite {
  test("SplitMix64.apply") {
    assertEquals(SplitMix64(42), SplitMix64(-9148929187392628276L, -4767286540954276203L))
    assertEquals(SplitMix64(0, 0), SplitMix64(0, 1))
  }

  test("SplitMix64#value") {
    val splitmix0 = SplitMix64(1337)
    val xs = LazyList
      .iterate((splitmix0, 0L)) { case (splitmix, _) => (splitmix.next, splitmix.value) }
      .drop(1)
      .map(_._2)
      .take(3)
      .toSeq
    assertEquals(xs, Seq(0xb5c19e300e8b07b3L, 0xd600e0e216c0ac76L, 0xc54efc3b3cc5af29L))
  }

  test("SplitMix64#split") {
    val splitmix0 = SplitMix64(42)
    val (splitmix1, splitmix2) = splitmix0.split
    assertEquals(splitmix1, SplitMix64(-236758195591629066L, -4767286540954276203L))
    assertEquals(splitmix2, SplitMix64(1275548033995301424L, -7932261524025849303L))
  }

  test("SplitMix64#left") {
    val splitmix0 = SplitMix64(42)
    assertEquals(splitmix0.left, splitmix0.split._1)
  }

  test("SplitMix64#right") {
    val splitmix0 = SplitMix64(42)
    assertEquals(splitmix0.right, splitmix0.split._2)
  }

  test("SplitMix64#toString") {
    assertEquals(SplitMix64(0, 0).toString, "SplitMix64(0,1)")
  }
}
