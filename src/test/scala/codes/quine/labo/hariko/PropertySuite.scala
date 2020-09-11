package codes.quine.labo.hariko

import scala.concurrent.duration.Duration

import minitest.SimpleTestSuite

import data.Tree

object PropertySuite extends SimpleTestSuite {
  test("Result#isPass") {
    assertEquals(Property.Pass(0, 0).isPass, true)
    assertEquals(Property.CounterExample(0, 0, 0, null).isPass, false)
    assertEquals(Property.NoValue(0, 0).isPass, false)
    assertEquals(Property.Error(0, 0, 0, null, null).isPass, false)
    assertEquals(Property.Timeout(0, Duration("0s")).isPass, false)
  }

  test("Property.check") {
    val result1 = Property.check(Param(0))((x: Boolean) => x || !x)
    assertEquals(result1, Property.Pass(0, 100))
    assertEquals(result1.toString, "pass (seed: 0x0, test: 100)")

    val result2 = Property.check(Param(0))((x: Boolean) => x && !x)
    assertEquals(result2, Property.CounterExample(0, 1, 0, false))
    assertEquals(
      result2.toString,
      """|counter example (seed: 0x0, test: 1, shrink: 0)
         |
         |value: false
         |""".stripMargin
    )

    val result3 = Property.checkWith(Gen.empty[Int], Param(0))(_ => true)
    assertEquals(result3, Property.NoValue(0, 1))
    assertEquals(result3.toString, "no value (seed: 0x0, test: 1)")

    val result4 = Property.check(Param(0, timeout = Duration("100ms"))) { (_: Unit) =>
      Thread.sleep(100)
      true
    }
    assertEquals(result4, Property.Timeout(0, Duration("100ms")))
    assertEquals(result4.toString, "timeout (seed: 0x0, duration: 100 milliseconds)")

    val result5 = Property.check(Param(0, minScale = 100, timeout = Duration("150ms"))) { (_: Int) =>
      Thread.sleep(100)
      false
    }
    assertEquals(result5, Property.Timeout(0, Duration("150ms")))
    assertEquals(result5.toString, "timeout (seed: 0x0, duration: 150 milliseconds)")

    val result6 = Property.check(Param(0)) { (_: Boolean) => sys.error("test") }
    assertEquals(
      result6.toString,
      """|error (seed: 0x0, test: 1, shrink: 0)
         |
         |value: false
         |
         |exception: java.lang.RuntimeException: test
         |""".stripMargin
    )

    val result7 = Property.check(Param(0, minScale = 100, timeout = Duration("150ms"))) { (_: Int) =>
      Thread.sleep(100)
      sys.error("test")
    }
    assertEquals(result7, Property.Timeout(0, Duration("150ms")))
    assertEquals(result7.toString, "timeout (seed: 0x0, duration: 150 milliseconds)")
  }

  test("Property.shrinkCounterExample") {
    trait TestCase {
      type T
      def gen: Gen[T]
      def param: Param
      def prop(x: T): Boolean
      def expected: Any
    }

    object TestCase {
      def apply[T0](g: Gen[T0], e: Any, p: Param = Param(42))(f: T0 => Boolean): TestCase =
        new TestCase {
          type T = T0
          def gen: Gen[T0] = g
          def param: Param = p
          def expected: Any = e
          def prop(x: T): Boolean = f(x)
        }
    }

    val testCases = Seq(
      TestCase(Gen.int(Range.linear(0, -100, 100)), 0, Param(0))(_ < 0),
      TestCase(Gen.int(Range.linear(0, 100)).filter(_ % 2 == 1), 1, Param(2))(_ < 0),
      TestCase(Gen.int(Range.linear(0, 100)).filter(_ % 2 == 1), 15, Param(2, maxDiscarded = 1))(_ < 0),
      TestCase(Gen.int(Range.linear(0, 100)).filter(_ % 2 == 1), 3, Param(2, maxDiscarded = 3))(_ < 0),
      TestCase(Gen[List[Int]], List.empty)(_.headOption.exists(_ < 0)),
      TestCase(Gen[List[Int]], List(0))(_.headOption.forall(_ < 0)),
      TestCase(Gen[String => Int], """{case "bar" => 0; case _ => -1}""") { f =>
        f("foo") == f("bar") || f("bar") == f("baz")
      },
      TestCase(Gen[(Int => Boolean) => Int], "{case {case -1147483649 => false} => 0; case _ => 1}", Param(1)) { f =>
        f(_ - 1000000000 >= 0) == f(_ >= 1000000000)
      }
    )

    // In this exceptional case, it returs result immediately as discarded.
    assertEquals(
      Property.shrinkCounterExample(Tree.pure(Option(0)), Param(0, maxDiscarded = 0))(_ => false),
      Some((0, 0))
    )

    for (testCase <- testCases) {
      val (_, tree) = testCase.gen.run(testCase.param.toRandom, testCase.param, testCase.param.maxScale)
      if (tree.value.forall(testCase.prop(_))) fail("root is not counter example")
      val result = Property.shrinkCounterExample(tree, testCase.param)(testCase.prop(_)).map(_._2).get
      // Compares via `toString` for function equality.
      assertEquals(result.toString, testCase.expected.toString)
    }
  }
}
