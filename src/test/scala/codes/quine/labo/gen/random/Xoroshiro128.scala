package codes.quine.labo.gen.random

import minitest.SimpleTestSuite

object Xoroshiro128Suite extends SimpleTestSuite {
  test("Xoroshiro128.apply") {
    val xoroshiro0 = Xoroshiro128(42)
    assertEquals(xoroshiro0, Xoroshiro128(-4767286540954276203L, 2949826092126892291L))
  }

  test("Xoroshiro128#value") {
    val xoroshiro0 = Xoroshiro128(-2152535657050944081L, 7960286522194355700L)
    assertEquals(xoroshiro0.value, 5807750865143411619L)
    val xoroshiro1 = xoroshiro0.next
    assertEquals(xoroshiro1.value, -2880618569221778578L)
  }
}
