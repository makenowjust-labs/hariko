package codes.quine.labo.hariko
package data

import minitest.SimpleTestSuite

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
  }

  test("Range#map") {
    assertEquals(Range.constant(0, -10, 10).map(_.toString).base, "0")
  }

  def equals[T](r1: Range[T], r2: Range[T])(implicit T: Ordering[T]): Boolean =
    (0 to 100).forall(k => r1.bounds(k) == r2.bounds(k))

  test("Range#map: Functor identity") {
    check(Property.forAll(DataGen.range(Gen.int)) { range =>
      equals(range.map(identity), range)
    })
  }

  test("Range#map: Functor composition") {
    val funGen = Gen.function1(Cogen.int, Gen.int)
    check(
      Property
        .forAll(Gen.tuple3(DataGen.range(Gen.int), funGen, funGen)) {
          case (range, f, g) =>
            equals(range.map(f).map(g), range.map(f.andThen(g)))
        }
        .withParam(_.copy(minSuccessful = 10))
    )
  }
}
