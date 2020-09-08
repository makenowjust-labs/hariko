package codes.quine.labo.hariko.data

package codes.quine.labo.hariko.data

import minitest.SimpleTestSuite

import UntupledFun._

object UntupledFunSuite extends SimpleTestSuite {
  val tupledFun2: ((Boolean, Boolean)) => Boolean = new Function1[(Boolean, Boolean), Boolean] {
    def apply(x: (Boolean, Boolean)): Boolean = true
    override def toString: String = "tupledFun2"
  }
  val tupledFun3: ((Boolean, Boolean, Boolean)) => Boolean = new Function1[(Boolean, Boolean, Boolean), Boolean] {
    def apply(x: (Boolean, Boolean, Boolean)): Boolean = true
    override def toString: String = "tupledFun3"
  }
  val tupledFun4: ((Boolean, Boolean, Boolean, Boolean)) => Boolean =
    new Function1[(Boolean, Boolean, Boolean, Boolean), Boolean] {
      def apply(x: (Boolean, Boolean, Boolean, Boolean)): Boolean = true
      override def toString: String = "tupledFun4"
    }

  test("UntupledFun2#apply") {
    val f = new UntupledFun2(tupledFun2)
    assertEquals(f(true, true), true)
  }

  test("UntupledFun2#toString") {
    val f = new UntupledFun2(tupledFun2)
    assertEquals(f.toString, "tupledFun2")
  }

  test("UntupledFun3#apply") {
    val f = new UntupledFun3(tupledFun3)
    assertEquals(f(true, true, true), true)
  }

  test("UntupledFun3#toString") {
    val f = new UntupledFun3(tupledFun3)
    assertEquals(f.toString, "tupledFun3")
  }

  test("UntupledFun4#apply") {
    val f = new UntupledFun4(tupledFun4)
    assertEquals(f(true, true, true, true), true)
  }

  test("UntupledFun4#toString") {
    val f = new UntupledFun4(tupledFun4)
    assertEquals(f.toString, "tupledFun4")
  }
}
