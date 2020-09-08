package codes.quine.labo.hariko.util

import minitest.SimpleTestSuite

object ShowSuite extends SimpleTestSuite {
  test("Show.any") {
    assertEquals(Show.any(1), "1")
    assertEquals(Show.any("foo"), "\"foo\"")
    assertEquals(Show.any("\b\f\n\r\t\\\"\'\u0000"), "\"\\b\\f\\n\\r\\t\\\\\\\"\\'\\u0000\"")
    assertEquals(Show.any('\''), "'\\''")
  }
}
