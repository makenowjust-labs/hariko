package codes.quine.labo.hariko.data

import scala.collection.immutable.SortedSet

import minitest.SimpleTestSuite

import PartialFun._

object PartialFunSuite extends SimpleTestSuite {
  test("PartialFun") {
    assertEquals(PartialFun, PartialFun)
  }

  test("PartialFun#map") {
    val f1 = Empty[Int, Int]()
    val f2 = Iso[Byte, Int, Int](_.toInt, _.toByte, f1)
    val f3 = Choice(f1, f2)
    val f4 = Uncurry(Empty[Int, Int :=> Int]())
    val f5 = Unlift(SortedSet(1, 2, 3), (_: Int) * 2)
    val f6 = Conquer(1)
    val f7 = Delay(() => Tree.pure(Option(f6: Unit :=> Int)))
    assertEquals(f1.map(_ + 1), f1)
    assertEquals(f2.map(_ + 1), f2)
    assertEquals(f3.map(_ + 1), f3)
    assertEquals(f4.map(_ + 1), f4)
    assertEquals(f5.map(_ + 1).asInstanceOf[Unlift[Int, Int]].f(2), 5)
    assertEquals(f6.map(_ + 1), Conquer(2))
    assertEquals(f7.map(_ + 1).asInstanceOf[Delay[Unit, Int]].delayedTree(), Tree.pure(Option(Conquer(2))))
  }

  test("PartialFun#table") {
    val f1 = Empty[Int, Int]()
    val f2 = Iso[Byte, Int, Int](_.toInt, _.toByte, f1)
    val f3 = Choice(f1, f2)
    val f4 = Uncurry(Empty[Int, Int :=> Int]())
    val f5 = Unlift(SortedSet(1, 2, 3), (_: Int) * 2)
    val f6 = Conquer(1)
    val f7 = Delay(() => Tree.pure(Option(f6: Unit :=> Int)))
    assertEquals(f1.table, LazyList.empty)
    assertEquals(f2.table, LazyList.empty)
    assertEquals(f3.table, LazyList.empty)
    assertEquals(f4.table, LazyList.empty)
    assertEquals(f5.table, LazyList(1 -> 2, 2 -> 4, 3 -> 6))
    assertEquals(f6.table, LazyList(() -> 1))
    assertEquals(f7.table, LazyList(() -> 1))
  }

  test("PartialFun#lift") {
    val f1 = Empty[Int, Int]()
    val f2 = Iso[Byte, Int, Int](_.toInt, _.toByte, f1)
    val f3 = Choice(f1, f2)
    val f4 = Uncurry(Empty[Int, Int :=> Int]())
    val f5 = Unlift(SortedSet(1, 2, 3), (_: Int) * 2)
    val f6 = Conquer(1)
    val f7 = Delay(() => Tree.pure(Option(f6: Unit :=> Int)))
    assertEquals(f1.lift(1), None)
    assertEquals(f2.lift(1), None)
    assertEquals(f3.lift(Left(1)), None)
    assertEquals(f4.lift((1, 2)), None)
    assertEquals(f5.lift(1), Some(2))
    assertEquals(f5.lift(4), None)
    assertEquals(f6.lift(()), Some(1))
    assertEquals(f7.lift(()), Some(1))
  }
}
