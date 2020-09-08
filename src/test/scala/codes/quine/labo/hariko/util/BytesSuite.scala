package codes.quine.labo.hariko
package util

import minitest.SimpleTestSuite

object BytesSuite extends SimpleTestSuite with HarikoChecker {
  test("Bytes.compose: coherence") {
    check(Property.forAll { x: (Byte, Byte) =>
      Bytes.decompose(Bytes.compose(x)) == x
    })
    check(Property.forAll { x: (Byte, Byte, Byte, Byte) =>
      Bytes.decompose(Bytes.compose(x)) == x
    })
    check(Property.forAll { x: (Int, Int) =>
      Bytes.decompose(Bytes.compose(x)) == x
    })
  }

  test("Bytes.decompose: coherence") {
    check(Property.forAll { x: Short =>
      Bytes.compose(Bytes.decompose(x)) == x
    })
    check(Property.forAll { x: Int =>
      Bytes.compose(Bytes.decompose(x)) == x
    })
    check(Property.forAll { x: Long =>
      Bytes.compose(Bytes.decompose(x)) == x
    })
  }
}
