package codes.quine.labo.hariko
package data

import minitest.SimpleTestSuite

import DataOps._
import HarikoData._

object RangeSuite extends SimpleTestSuite with HarikoChecker {
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

    assertEquals(Range.linear(0.0f, -1.0f, 1.0f).base, 0.0f)
    assertEquals(Range.linear(0.0f, 1.0f).base, 0.0f)

    assertEquals(Range.linear(0.0, -1.0, 1.0).base, 0.0)
    assertEquals(Range.linear(0.0, -1.0, 1.0).bounds(0), (0.0, 0.0))
    assertEquals(Range.linear(0.0, -1.0, 1.0).bounds(50), (-0.5, 0.5))
    assertEquals(Range.linear(0.0, -1.0, 1.0).bounds(100), (-1.0, 1.0))
    assertEquals(Range.linear(0.0, 1.0).base, 0.0)
    assertEquals(Range.linear(0.0, 1.0).bounds(0), (0.0, 0.0))
    assertEquals(Range.linear(0.0, 1.0).bounds(50), (0.0, 0.5))
    assertEquals(Range.linear(0.0, 1.0).bounds(100), (0.0, 1.0))
  }

  test("Range#map") {
    assertEquals(Range.constant(0, -10, 10).map(_.toString).base, "0")
  }

  test("Range#map: Functor identity") {
    check(Property.forAll { range: Range[Int] =>
      range.map(identity) === range
    })
  }

  test("Range#map: Functor composition") {
    val p = Property.forAll[(Range[Int], Int => Int, Int => Int)] {
      case (range, f, g) =>
        range.map(f).map(g) === range.map(f.andThen(g))
    }
    check(p.withParam(_.copy(minSuccessful = 10)))
  }
}
