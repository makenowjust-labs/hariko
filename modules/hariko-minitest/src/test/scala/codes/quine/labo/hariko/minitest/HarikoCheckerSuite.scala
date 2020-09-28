package codes.quine.labo.hariko
package minitest

import _root_.minitest.SimpleTestSuite
import _root_.minitest.api.AssertionException

object HarikoCheckerSuite extends SimpleTestSuite with HarikoChecker {
  test("HarikoChecker#check") {
    check(Property.forAll((x: Int) => x < 0 || x >= 0))
    val ex = intercept[AssertionException] {
      check(Property.forAll((x: Int) => x < 0))
    }
    assert(ex.getMessage().contains("value: 0"))
  }

  test("HarikoChecker#checkCoverage") {
    checkCoverage[Int](
      (1, "positive") -> (_ > 0),
      (1, "negative") -> (_ < 0)
    )
    val ex = intercept[AssertionException] {
      checkCoverage[Int](
        (0, "positive") -> (_ > 0),
        (1, "negative") -> (_ < 0)
      )
    }
    assert(ex.getMessage().contains("coverage invalid: positive"))
  }

  test("HarikoChecker#checkCoverageWith") {
    checkCoverageWith(Gen.int(Range.linear(0, Int.MinValue, 0)))(
      (0, "positive") -> (_ > 0),
      (1, "negative") -> (_ < 0)
    )
  }
}
