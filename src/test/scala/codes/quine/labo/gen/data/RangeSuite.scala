package codes.quine.labo.gen.data

import minitest.SimpleTestSuite

object RangeSuite extends SimpleTestSuite {
  test("Range.constant") {
    assertEquals(Range.constant(0, -10, 10).base, 0)
    assertEquals(Range.constant(0, -10, 10).bounds(0), (-10, 10))
    assertEquals(Range.constant(0, -10, 10).bounds(100), (-10, 10))
    assertEquals(Range.constant(0, 10).base, 0)
    assertEquals(Range.constant(0, 10).bounds(0), (0, 10))
    assertEquals(Range.constant(0, 10).bounds(100), (0, 10))
  }

  test("Range.linear") {
    assertEquals(Range.linear(0.toByte, -10.toByte, 10.toByte).base, 0.toByte)
    assertEquals(Range.linear(0.toByte, 10.toByte).base, 0.toByte)

    assertEquals(Range.linear(0.toShort, -10.toShort, 10.toShort).base, 0.toShort)
    assertEquals(Range.linear(0.toShort, 10.toShort).base, 0.toShort)

    assertEquals(Range.linear(0, -10, 10).base, 0)
    assertEquals(Range.linear(0, -10, 10).bounds(0), (0, 0))
    assertEquals(Range.linear(0, -10, 10).bounds(50), (-5, 5))
    assertEquals(Range.linear(0, -10, 10).bounds(100), (-10, 10))
    assertEquals(Range.linear(0, 10).base, 0)
    assertEquals(Range.linear(0, 10).bounds(0), (0, 0))
    assertEquals(Range.linear(0, 10).bounds(50), (0, 5))
    assertEquals(Range.linear(0, 10).bounds(100), (0, 10))

    assertEquals(Range.linear(0L, Long.MinValue, Long.MaxValue).bounds(100), (Long.MinValue, Long.MaxValue))
    assertEquals(Range.linear(Long.MinValue, Long.MaxValue).bounds(100), (Long.MinValue, Long.MaxValue))
  }

  test("Range#map") {
    assertEquals(Range.constant(0, -10, 10).map(_.toString).base, "0")
  }
}
