package codes.quine.labo.hariko

import minitest.SimpleTestSuite

import data.Tree
import HarikoOps._

object GenSuite extends SimpleTestSuite with HarikoChecker {
  test("Gen#map") {
    val gen = Gen.int(Range.linear(0, 10)).map(_ + 1)
    checkCoverageWith(gen)(
      (0, "zero") -> (_ == 0),
      (1, "non-zero") -> (_ > 0)
    )
  }

  test("Gen#filter") {
    val gen = Gen.int(Range.linear(0, 100)).filter(_ % 2 == 0)
    checkCoverageWith(gen)(
      (0, "odd") -> (_ % 2 == 1),
      (1, "even") -> (_ % 2 == 0)
    )
  }

  test("Gen#mapFilter") {
    val gen =
      Gen.int(Range.linear(0, 100)).mapFilter(k => if (k % 2 == 0) Some(k.toString) else None)
    checkCoverageWith(gen)(
      (0, "odd") -> (_.toInt % 2 == 1),
      (1, "even") -> (_.toInt % 2 == 0)
    )
  }

  test("Gen#collect") {
    val gen =
      Gen.int(Range.linear(0, 100)).collect { case k if k % 2 == 0 => k.toString }
    checkCoverageWith(gen)(
      (0, "odd") -> (_.toInt % 2 == 1),
      (1, "even") -> (_.toInt % 2 == 0)
    )
  }

  test("Gen.of") {
    val gen = Gen.of((rand, _, _) => (rand, Tree.pure(Option(1))))
    val param = Param(0)
    assertEquals(gen.run(param.toRandom, param, 100), (param.toRandom, Tree.pure(Option(1))))
  }

  test("Gen.delay") {
    def listGen: Gen[List[Boolean]] =
      Gen.delay(Gen.frequency(3 -> Gen.pure(Nil), 2 -> Gen.map2(Gen.boolean, listGen)(_ :: _)))
    val gen = listGen
    assert(gen === Gen.delay(listGen))
  }

  test("Gen.pure") {
    val gen = Gen.pure(1)
    val param = Param(0)
    assertEquals(gen.run(param.toRandom, param, 100), (param.toRandom, Tree.pure(Option(1))))
  }

  test("Gen.empty") {
    val gen = Gen.empty[Int]
    val param = Param(0)
    assertEquals(gen.run(param.toRandom, param, 100), (param.toRandom, Tree.pure(None)))
  }

  test("Gen.map2") {
    val param = Param(0)

    val gen1 = Gen.map2(Gen.pure(1), Gen.pure(2))(_ + _)
    assertEquals(gen1.run(param.toRandom, param, 100), (param.toRandom, Tree.pure(Some(3))))

    val gen2 = Gen.map2(Gen.pure(1), Gen.empty[Int])(_ + _)
    assertEquals(gen2.run(param.toRandom, param, 100), (param.toRandom, Tree.pure(None)))
  }

  test("Gen.map3") {
    val gen1 =
      Gen.map2(Gen.map2(Gen.int, Gen.int)((_, _)), Gen.int) { case ((x1, x2), x3) => (x1, x2, x3) }
    val gen2 =
      Gen.map2(Gen.int, Gen.map2(Gen.int, Gen.int)((_, _))) { case (x1, (x2, x3)) => (x1, x2, x3) }
    val gen3 = Gen.map3(Gen.int, Gen.int, Gen.int)((_, _, _))
    assert(gen1 === gen3)
    assert(gen2 === gen3)
  }

  test("Gen.map4") {
    val gen1 =
      Gen.map2(Gen.tuple2(Gen.int, Gen.int), Gen.tuple2(Gen.int, Gen.int)) {
        case ((x1, x2), (x3, x4)) => (x1, x2, x3, x4)
      }
    val gen2 =
      Gen.map4(Gen.int, Gen.int, Gen.int, Gen.int)((_, _, _, _))
    assert(gen1 === gen2)
  }

  test("Gen.replicate") {
    val gen = Gen.replicate(5, Gen.int)
    check(Property.forAllWith(gen)(_.size == 5))
  }

  test("Gen.setReplicate") {
    val gen = Gen.setReplicate(5, Gen.int)
    check(Property.forAllWith(gen)(_.distinct.size == 5))
  }

  test("Gen.frequency") {
    val gen = Gen.frequency(1 -> Gen.pure(1), 2 -> Gen.pure(2))
    checkCoverageWith(gen)(
      (1, "Gen.pure(1)") -> (_ == 1),
      (1, "Gen.pure(2)") -> (_ == 2)
    )
  }

  test("Gen.unit") {
    val gen = Gen.unit
    val param = Param(0)
    assertEquals(gen.run(param.toRandom, param, 100), (param.toRandom, Tree.pure(Some(()))))
  }

  test("Gen.boolean") {
    checkCoverage[Boolean](
      (1, "true") -> (_ == true),
      (1, "false") -> (_ == false)
    )
  }

  test("Gen.byte") {
    check(Property.forAllWith(Gen.byte(Range.linear(0, Byte.MaxValue)))(_ >= 0))
  }

  test("Gen.short") {
    check(Property.forAllWith(Gen.short(Range.linear(0, Short.MaxValue)))(_ >= 0))
  }

  test("Gen.int") {
    check(Property.forAllWith(Gen.int(Range.linear(0, Int.MaxValue)))(_ >= 0))
  }

  test("Gen.long") {
    check(Property.forAllWith(Gen.long(Range.linear(0, Long.MaxValue)))(_ >= 0))
  }

  test("Gen.char") {
    check(Property.forAll((x: Char) => ' ' <= x && x <= '~'))
  }

  test("Gen.float") {
    check(Property.forAll((x: Float) => !x.isNaN && !x.isInfinite))
  }

  test("Gen.double") {
    check(Property.forAll((x: Double) => !x.isNaN && !x.isInfinite))
  }

  test("Gen.string") {
    check(Property.forAll((x: String) => x.size <= 30))
  }

  test("Gen.list") {
    check(Property.forAll((xs: List[Boolean]) => xs.size <= 30))
  }

  test("Gen.set") {
    check(Property.forAll((xs: Set[Int]) => xs.size <= 30))
  }

  test("Gen.map") {
    check(Property.forAll((x: Map[Int, Boolean]) => x.size <= 30))
  }

  test("Gen.option") {
    checkCoverage[Option[Boolean]](
      (1, "None") -> (_.isEmpty),
      (1, "Some") -> (_.isDefined)
    )
  }

  test("Gen.either") {
    checkCoverage[Either[Boolean, Boolean]](
      (1, "Left") -> (_.isLeft),
      (1, "Right") -> (_.isRight)
    )
  }

  test("Gen.tuple2") {
    checkCoverage[(Boolean, Boolean)](
      (1, "_1") -> (_._1),
      (1, "_2") -> (_._2)
    )
  }

  test("Gen.tuple3") {
    checkCoverage[(Boolean, Boolean, Boolean)](
      (1, "_1") -> (_._1),
      (1, "_2") -> (_._2),
      (1, "_3") -> (_._3)
    )
  }

  test("Gen.tuple4") {
    checkCoverage[(Boolean, Boolean, Boolean, Boolean)](
      (1, "_1") -> (_._1),
      (1, "_2") -> (_._2),
      (1, "_3") -> (_._3),
      (1, "_4") -> (_._4)
    )
  }

  test("Gen.function1") {
    checkCoverage[Boolean => Boolean](
      (1, "identity") -> (f => f(true) && !f(false)),
      (1, "not") -> (f => !f(true) && f(false)),
      (1, "const(true)") -> (f => f(true) && f(false)),
      (1, "const(false)") -> (f => !f(true) && !f(false))
    )
  }

  test("Gen.function2") {
    checkCoverage[(Boolean, Boolean) => Boolean](
      (1, "and") -> (f => f(true, true) && !f(true, false) && !f(false, true) && !f(false, false)),
      (1, "or") -> (f => f(true, true) && f(true, false) && f(true, false) && !f(false, false))
    )
  }

  test("Gen.function3") {
    checkCoverage[(Boolean, Boolean, Boolean) => Boolean](
      (1, "and-like") -> (f => f(true, true, true)),
      (1, "or-like") -> (f => !f(false, false, false))
    )
  }

  test("Gen.function4") {
    checkCoverage[(Boolean, Boolean, Boolean, Boolean) => Boolean](
      (1, "and-like") -> (f => f(true, true, true, true)),
      (1, "or-like") -> (f => !f(false, false, false, false))
    )
  }
}
