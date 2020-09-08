package codes.quine.labo.hariko.data

import minitest.SimpleTestSuite

object LocalFunSuite extends SimpleTestSuite {
  test("LocalFun#apply") {
    val f = new LocalFun(1, 2)
    intercept[RuntimeException](f(2))
    assertEquals(f(1), 2)
  }

  test("LocalFun#toString") {
    val f = new LocalFun(1, 2)
    assertEquals(f.toString, "{case 1 => 2}")
  }
}
