package codes.quine.labo.hariko

import minitest.SimpleTestSuite

object CogenSuite extends SimpleTestSuite with HarikoChecker {
  test("Cogen.apply") {
    assert(Cogen[Unit].isInstanceOf[Cogen[Unit]])
  }

  test("Cogen.empty") {
    check(Property.forAllWith(Gen.tuple3(Gen.function1(Cogen.empty[Int], Gen.int), Gen.int, Gen.int)) {
      case (f, x, y) => f(x) == f(y)
    })
  }

  test("Cogen.conquer") {
    val fs1 = Gen.function1(Cogen.boolean, Gen.int).samples(paramForCoverage)
    val fs2 = Gen
      .function1(Cogen.tuple2(Cogen.conquer, Cogen.boolean), Gen.int)
      .map(f => (x: Boolean) => f(((), x)))
      .samples(paramForCoverage)
    val fs3 = Gen
      .function1(Cogen.tuple2(Cogen.boolean, Cogen.conquer), Gen.int)
      .map(f => (x: Boolean) => f((x, ())))
      .samples(paramForCoverage)
    assert {
      fs1
        .zip(fs2)
        .zip(fs3)
        .map { case ((f1, f2), f3) => (f1, f2, f3) }
        .take(100)
        .forall { case (f1, f2, f3) =>
          Seq(true, false).forall(x => f1(x) == f2(x) && f2(x) == f3(x))
        }
    }
  }

  test("Cogen.unit") {
    checkCoverage[Unit => Boolean](
      (1, "true") -> (f => f(())),
      (1, "false") -> (f => f(()))
    )
  }

  test("Cogen.boolean") {
    checkCoverage[Boolean => Int](
      (1, "f(true) >= 0") -> (f => f(true) >= 0),
      (1, "f(false) < 0") -> (f => f(false) < 0)
    )
  }

  test("Cogen.byte") {
    checkCoverage[Byte => Boolean](
      (1, "f(0) != f(127)") -> (f => f(0) != f(127)),
      (1, "f(0) != f(-128)") -> (f => f(0) != f(-128))
    )
  }

  test("Cogen.short") {
    checkCoverage[Short => Boolean](
      (1, "f(0) != f(Short.MinValue)") -> (f => f(0) != f(Short.MinValue)),
      (1, "f(0) != f(Short.MaxValue)") -> (f => f(0) != f(Short.MaxValue))
    )
  }

  test("Cogen.int") {
    checkCoverage[Int => Boolean](
      (1, "f(0) != f(Int.MinValue)") -> (f => f(0) != f(Int.MinValue)),
      (1, "f(0) != f(Int.MaxValue)") -> (f => f(0) != f(Int.MaxValue))
    )
  }

  test("Cogen.long") {
    checkCoverage[Long => Boolean](
      (1, "f(0) != f(Long.MinValue)") -> (f => f(0) != f(Long.MinValue)),
      (1, "f(0) != f(Long.MaxValue)") -> (f => f(0) != f(Long.MaxValue))
    )
  }

  test("Cogen.char") {
    checkCoverage[Char => Boolean](
      (1, "f('a') != f('z')") -> (f => f('a') != f('z')),
      (1, "f('A') != f('Z')") -> (f => f('A') != f('Z'))
    )
  }

  test("Cogen.float") {
    checkCoverage[Float => Boolean](
      (1, "f(0.0f) != f(-1.0f)") -> (f => f(0.0f) != f(-1.0f)),
      (1, "f(0.0f) != f(1.0f)") -> (f => f(0.0f) != f(1.0f))
    )
  }

  test("Cogen.double") {
    checkCoverage[Double => Boolean](
      (1, "f(0.0) != f(-1.0)") -> (f => f(0.0) != f(-1.0)),
      (1, "f(0.0) != f(1.0)") -> (f => f(0.0) != f(1.0))
    )
  }

  test("Cogen.string") {
    checkCoverage[String => Boolean](
      (1, "f(\"fizzbuzz\") != f(\"fizz\")") -> (f => f("fizzbuzz") != f("fizz")),
      (1, "f(\"fizzbuzz\") != f(\"buzz\")") -> (f => f("fizzbuzz") != f("buzz"))
    )
  }

  test("Cogen.list") {
    checkCoverage[List[Int] => Boolean](
      (1, "f(List(1, 2, 3)) != f(List(4, 5, 6))") -> (f => f(List(1, 2, 3)) != f(List(4, 5, 6))),
      (1, "f(List(3, 2, 1)) != f(List(6, 5, 4))") -> (f => f(List(3, 2, 1)) != f(List(6, 5, 4)))
    )
  }

  test("Cogen.set") {
    // NOTE: `Set(1, 2).toList == List(1, 2)`, but `Set(2, 1).toList == List(2, 1)`.
    check(Property.forAll { (f: Set[Int] => Int) =>
      f(Set(1, 2)) == f(Set(2, 1))
    })
    checkCoverage[Set[Int] => Boolean](
      (1, "f(Set(1, 2, 3)) != f(Set(4, 5, 6))") -> (f => f(Set(1, 2, 3)) != f(Set(4, 5, 6))),
      (1, "f(Set(4, 5, 6)) != f(Set(7, 8, 9))") -> (f => f(Set(4, 5, 6)) != f(Set(7, 8, 9)))
    )
  }

  test("Cogen.map") {
    // NOTE: `Map(1 -> 2, 2 -> 3).toList == List(1 -> 2, 2 -> 3)`, but `Map(2 -> 3, 1 -> 2).toList == List(2 -> 3, 1 -> 2)`.
    check(Property.forAll { (f: Map[Int, Int] => Int) =>
      f(Map(1 -> 2, 2 -> 3)) == f(Map(2 -> 3, 1 -> 2))
    })
    checkCoverage[Map[Int, Int] => Boolean](
      (1, "f(Map(1 -> 2)) != f(Map(1 -> 3))") -> (f => f(Map(1 -> 2)) != f(Map(1 -> 3))),
      (1, "f(Map(1 -> 2)) != f(Map(3 -> 4))") -> (f => f(Map(1 -> 2)) != f(Map(3 -> 4)))
    )
  }

  test("Cogen.option") {
    checkCoverage[Option[Int] => Boolean](
      (1, "f(None) != f(Some(1))") -> (f => f(None) != f(Some(1))),
      (1, "f(Some(1)) != f(Some(2))") -> (f => f(Some(1)) != f(Some(2)))
    )
  }

  test("Cogen.either") {
    checkCoverage[Either[Int, Int] => Boolean](
      (1, "f(Left(1)) != f(Right(1))") -> (f => f(Left(1)) != f(Right(1))),
      (1, "f(Left(1)) != f(Left(2))") -> (f => f(Left(1)) != f(Left(2))),
      (1, "f(Right(1)) != f(Right(2))") -> (f => f(Right(1)) != f(Right(2)))
    )
  }

  test("Cogen.tuple2") {
    checkCoverage[((Int, Int)) => Boolean](
      (1, "f((0, 1)) != f((1, 0))") -> (f => f((0, 1)) != f((1, 0))),
      (1, "f((0, 0)) != f((1, 1))") -> (f => f((0, 1)) != f((1, 1)))
    )
  }

  test("Cogen.tuple3") {
    checkCoverage[((Int, Int, Int)) => Boolean](
      (1, "f((0, 0, 1)) != f((1, 0, 0))") -> (f => f((0, 0, 1)) != f((1, 0, 0))),
      (1, "f((0, 1, 0)) != f((0, 0, 1))") -> (f => f((0, 1, 0)) != f((0, 0, 1))),
      (1, "f((0, 0, 0)) != f((1, 1, 1))") -> (f => f((0, 0, 0)) != f((1, 1, 1)))
    )
  }

  test("Cogen.tuple4") {
    checkCoverage[((Int, Int, Int, Int)) => Boolean](
      (1, "f((0, 0, 0, 1)) != f((1, 0, 0, 0))") -> (f => f((0, 0, 0, 1)) != f((1, 0, 0, 0))),
      (1, "f((0, 1, 0, 0)) != f((0, 0, 1, 0))") -> (f => f((0, 1, 0, 0)) != f((0, 0, 1, 0))),
      (1, "f((0, 0, 0, 0)) != f((1, 1, 1, 1))") -> (f => f((0, 0, 0, 0)) != f((1, 1, 1, 1)))
    )
  }

  test("Cogen.function1") {
    checkCoverage[(Int => Boolean) => Boolean](
      (1, "f(_ >= 0) != f(_ < 0)") -> (f => f(_ >= 0) != f(_ < 0)),
      (1, "f(_ >= 150000) != f(_ <= 50000)") -> (f => f(_ >= 150000) != f(_ <= 50000))
    )
  }

  test("Cogen.function2") {
    checkCoverage[((Boolean, Boolean) => Boolean) => Boolean](
      (1, "f(_ && _) != f(_ || _)") -> (f => f(_ && _) != f(_ || _)),
      (1, "f(!_ && _) != f(_ || !_)") -> (f => f(!_ && _) != f(_ || !_))
    )
  }

  test("Cogen.function3") {
    checkCoverage[((Boolean, Boolean, Boolean) => Boolean) => Boolean](
      (1, "f(_ && _ && _) != f(_ || _ || _)") -> (f => f(_ && _ && _) != f(_ || _ || _)),
      (1, "f(_ && _ || _) != f(_ || _ && _)") -> (f => f(_ && _ || _) != f(_ || _ && _))
    )
  }

  test("Cogen.function4") {
    checkCoverage[((Boolean, Boolean, Boolean, Boolean) => Boolean) => Boolean](
      (1, "f(_ && _ && _ && _) != f(_ || _ || _ || _)") -> (f => f(_ && _ && _ && _) != f(_ || _ || _ || _)),
      (1, "f(_ && _ || _ && _) != f(_ || _ && _ || _)") -> (f => f(_ && _ || _ && _) != f(_ || _ && _ || _))
    )
  }
}
