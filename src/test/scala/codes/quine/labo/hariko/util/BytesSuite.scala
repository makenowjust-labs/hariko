package codes.quine.labo.hariko
package util

import minitest.SimpleTestSuite

object BytesSuite extends SimpleTestSuite with HarikoChecker {
  test("Bytes.compose: coherence") {
    val byteGen = Gen.byte(Range.linear(0.toByte, -128.toByte, 127.toByte))
    check(Property.forAll(Gen.tuple2(byteGen, byteGen)) { x =>
      Bytes.decompose(Bytes.compose(x)) == x
    })
    check(Property.forAll(Gen.tuple4(byteGen, byteGen, byteGen, byteGen)) { x =>
      Bytes.decompose(Bytes.compose(x)) == x
    })
    check(Property.forAll(Gen.tuple2(Gen.int, Gen.int)) { x =>
      Bytes.decompose(Bytes.compose(x)) == x
    })
  }

  test("Bytes.decompose: coherence") {
    check(Property.forAll(Gen.short(Range.linear(0.toShort, Short.MinValue, Short.MaxValue))) { x =>
      Bytes.compose(Bytes.decompose(x)) == x
    })
    check(Property.forAll(Gen.int) { x =>
      Bytes.compose(Bytes.decompose(x)) == x
    })
    check(Property.forAll(Gen.long(Range.linear(0, Long.MinValue, Long.MaxValue))) { x =>
      Bytes.compose(Bytes.decompose(x)) == x
    })
  }
}
