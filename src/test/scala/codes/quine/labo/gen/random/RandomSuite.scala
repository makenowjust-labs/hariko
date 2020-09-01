package codes.quine.labo.gen.random

import minitest.SimpleTestSuite

object RandomSuite extends SimpleTestSuite {
  val Seeds = Seq(0, 42, -42)
  val IterationNumber = 100

  test("Random#nextLong") {
    def assertBounds(rand: Random, bounds: (Long, Long)): Unit = {
      val (min, max) = bounds
      assert(min <= max)
      val xs = LazyList.iterate((rand, 0L))(_._1.nextLong(bounds)).map(_._2).drop(1)
      assert(xs.take(IterationNumber).forall(x => min <= x && x <= max))
    }

    for (seed <- Seeds) {
      val rand = Random(seed)
      assertBounds(rand, (50, 100))
      assertBounds(rand, (-100, -50))
      assertBounds(rand, (-100, 100))
      assertBounds(rand, (Long.MinValue, 0))
      assertBounds(rand, (0, Long.MaxValue))
      assertBounds(rand, (Long.MinValue, Long.MaxValue))
      assertBounds(rand, (Long.MinValue + 100, Long.MaxValue - 100))
    }
  }

  test("Random#nextDouble") {
    for (seed <- Seeds) {
      val rand = Random(seed)
      val xs = LazyList.iterate((rand, 0.0))(_._1.nextDouble).map(_._2).drop(1)
      assert(xs.take(IterationNumber).forall(x => 0.0 <= x && x <= 1.0))
    }

    def assertBounds(rand: Random, bounds: (Double, Double)): Unit = {
      val (min, max) = bounds
      assert(min <= max)
      val xs = LazyList.iterate((rand, 0.0))(_._1.nextDouble(bounds)).map(_._2).drop(1)
      assert(xs.take(IterationNumber).forall(x => min <= x && x <= max))
    }

    for (seed <- Seeds) {
      val rand = Random(seed)
      assertBounds(rand, (-1.0, 1.0))
      assertBounds(rand, (1e10, 1e20))
      assertBounds(rand, (1e-20, 1e-10))
      assertBounds(rand, (-1e20, 1e10))
      assertBounds(rand, (-1e-10, -1e-20))
    }
  }

  test("Random#split") {
    val rand0 = Random(0)
    val (rand1, rand2) = rand0.split
    assertEquals(rand1.splitmix, rand0.splitmix.left)
    assertEquals(rand2.splitmix, rand0.splitmix.right)
  }

  test("Random#left") {
    val rand0 = Random(0)
    assertEquals(rand0.left, rand0.split._1)
  }

  test("Random#right") {
    val rand0 = Random(0)
    assertEquals(rand0.right, rand0.split._2)
  }
}
