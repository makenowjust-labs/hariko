package codes.quine.labo.hariko

import minitest.SimpleTestSuite

import random.Random

object ParamSuite extends SimpleTestSuite {
  test("Param#toRandom") {
    val param = Param(42)
    assertEquals(param.toRandom, Random(param.seed))
  }
}
