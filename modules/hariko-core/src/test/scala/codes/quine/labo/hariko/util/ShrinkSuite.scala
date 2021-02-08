package codes.quine.labo.hariko
package util

import minitest.SimpleTestSuite

import scala.collection.immutable.SortedSet

import data.Tree
import data.PartialFun._

object ShrinkSuite extends SimpleTestSuite {
  test("Shrink.long") {
    assertEquals(Shrink.long(0, 0).toList, List.empty)
    assertEquals(Shrink.long(0, 1).toList, List(0))
    assertEquals(Shrink.long(0, -1).toList, List(0))
    assertEquals(Shrink.long(0, 16).toList, List(8, 12, 14, 15))
    assertEquals(Shrink.long(0, 15).toList, List(8, 12, 14))
    assertEquals(Shrink.long(0, 14).toList, List(7, 11, 13))
    assertEquals(Shrink.long(5, 10).toList, List(7, 9))
    assertEquals(Shrink.long(10, 5).toList, List(8, 6))
  }

  test("Shrink.double") {
    assertEquals(Shrink.double(0.0, 0.0).toList, List.empty)
    assertEquals(Shrink.double(0.0, 1.0).size, 32)
    assertEquals(Shrink.double(0.0, 1.0).take(2).toList, List(0.5, 0.75))
  }

  test("Shrink.list") {
    assertEquals(Shrink.list(0, List.empty).toList, List())
    assertEquals(Shrink.list(0, List(1)).toList, List(List.empty))
    assertEquals(Shrink.list(0, List(1, 2, 3)).toList, List(List(1), List(2, 3), List(1, 2), List(1, 3)))
    assertEquals(Shrink.list(2, List(1, 2, 3)).toList, List(List(2, 3), List(1, 2), List(1, 3)))
    assertEquals(
      Shrink.list(3, List(1, 2, 3, 4)).toList,
      List(List(1, 2, 3), List(1, 3, 4), List(1, 2, 4), List(2, 3, 4))
    )
  }

  test("Shrink.partialFun") {
    assertEquals(Shrink.partialFun(Empty[Int, Option[Tree[Int]]]()), LazyList.empty)

    val xs = Shrink.partialFun[Boolean, Int](
      Unlift(SortedSet(false, true), (_: Boolean) => Some(Tree(1, LazyList(Tree.pure(2)))))
    )
    assertEquals(xs.size, 4)
    assertEquals(
      xs.map(_.asInstanceOf[Unlift[Boolean, _]].domain),
      LazyList(SortedSet(false), SortedSet(true), SortedSet(false, true), SortedSet(false, true))
    )
  }
}
