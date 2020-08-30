package codes.quine.labo.gen.random

import minitest.SimpleTestSuite

object SplitMix64Suite extends SimpleTestSuite {
  test("SplitMix64#value") {
    val splitmix0 = SplitMix64(0)
    assertEquals(splitmix0.value, -2152535657050944081L)
    val splitmix1 = splitmix0.next
    assertEquals(splitmix1.value, 7960286522194355700L)
  }
}
