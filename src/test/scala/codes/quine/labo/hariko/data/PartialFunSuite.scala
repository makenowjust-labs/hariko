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
    assertEquals(f1.map(_ + 1), f1)
    assertEquals(f2.map(_ + 1), f2)
    assertEquals(f3.map(_ + 1), f3)
    assertEquals(f4.map(_ + 1), f4)
    assertEquals(f5.map(_ + 1).asInstanceOf[Unlift[Int, Int]].f(2), 5)
  }

  test("PartialFun#table") {
    val f1 = Empty[Int, Int]()
    val f2 = Iso[Byte, Int, Int](_.toInt, _.toByte, f1)
    val f3 = Choice(f1, f2)
    val f4 = Uncurry(Empty[Int, Int :=> Int]())
    val f5 = Unlift(SortedSet(1, 2, 3), (_: Int) * 2)
    assertEquals(f1.table, LazyList.empty)
    assertEquals(f2.table, LazyList.empty)
    assertEquals(f3.table, LazyList.empty)
    assertEquals(f4.table, LazyList.empty)
    assertEquals(f5.table, LazyList(1 -> 2, 2 -> 4, 3 -> 6))
  }

  test("PartialFun#lift") {
    val f1 = Empty[Int, Int]()
    val f2 = Iso[Byte, Int, Int](_.toInt, _.toByte, f1)
    val f3 = Choice(f1, f2)
    val f4 = Uncurry(Empty[Int, Int :=> Int]())
    val f5 = Unlift(SortedSet(1, 2, 3), (_: Int) * 2)
    assertEquals(f1.lift(1), None)
    assertEquals(f2.lift(1), None)
    assertEquals(f3.lift(Left(1)), None)
    assertEquals(f4.lift((1, 2)), None)
    assertEquals(f5.lift(1), Some(2))
    assertEquals(f5.lift(4), None)
  }
}
