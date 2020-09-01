package codes.quine.labo.gen.data

import minitest.SimpleTestSuite

object TreeSuite extends SimpleTestSuite {
  test("Tree.pure") {
    assertEquals(Tree.pure(1).value, 1)
    assertEquals(Tree.pure(1).children, LazyList.empty)
  }

  test("Tree.map2") {
    val t1 = Tree(1, LazyList(Tree.pure(0)))
    val t2 = Tree(2, LazyList(Tree.pure(0)))
    assertEquals(
      Tree.map2(t1, t2)((_, _)),
      Tree((1, 2), LazyList(Tree((0, 2), LazyList(Tree.pure((0, 0)))), Tree((1, 0), LazyList(Tree.pure((0, 0))))))
    )
    assertEquals(
      Tree.map2(t1, t2)(_ + _),
      Tree(3, LazyList(Tree(2, LazyList(Tree.pure(0))), Tree(1, LazyList(Tree.pure(0)))))
    )
  }

  test("Tree#map") {
    val t = Tree(1, LazyList(Tree.pure(2), Tree.pure(3)))
    assertEquals(t.map(_ + 1), Tree(2, LazyList(Tree.pure(3), Tree.pure(4))))
  }

  test("Tree#expand") {
    val t = Tree(2, LazyList(Tree.pure(1)))
    assertEquals(t.expand(x => if (x == 1) Seq.empty else Seq(x - 1)), Tree(2, LazyList(Tree.pure(1), Tree.pure(1))))
  }
}
