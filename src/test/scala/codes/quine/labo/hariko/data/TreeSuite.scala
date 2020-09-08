package codes.quine.labo.hariko
package data

import minitest.SimpleTestSuite

import HarikoData._

object TreeSuite extends SimpleTestSuite with HarikoChecker {
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
    val t0 = Tree(2, LazyList(Tree.pure(1)))
    val t1 = t0.expand(x => if (x == 1) Seq.empty else Seq(x - 1))
    assertEquals(t1, Tree(2, LazyList(Tree(1, LazyList(Tree.pure(1))), Tree.pure(1))))
  }

  test("Tree#collapse") {
    val t = Tree(Option(1), LazyList(Tree(None, LazyList(Tree.pure(Option(1)))), Tree.pure(Option(2))))
    assertEquals(Tree.collapse(t), Some(Tree(1, LazyList(Tree.pure(2)))))
    assertEquals(Tree.collapse(t.children(0)), None)
  }

  test("Tree#map: Functor identity") {
    check(Property.forAll((t: Tree[Boolean]) => t.map(identity) == t))
  }

  test("Tree#map: Functor composition") {
    check(Property.forAll[(Tree[Boolean], Boolean => Boolean, Boolean => Boolean)] {
      case (t, f, g) =>
        t.map(f).map(g) == t.map(f.andThen(g))
    })
  }

  test("Tree.pure: Applicative left unit") {
    val unit = Tree.pure(())
    check(Property.forAll { t: Tree[Boolean] =>
      Tree.map2(unit, t)((_, x) => x) == t
    })
  }

  test("Tree.pure: Applicative right unit") {
    val unit = Tree.pure(())
    check(Property.forAll { t: Tree[Boolean] =>
      Tree.map2(t, unit)((x, _) => x) == t
    })
  }

  test("Tree.map2: Applicative associative") {
    check(Property.forAll[(Tree[Boolean], Tree[Boolean], Tree[Boolean])] {
      case (t1, t2, t3) =>
        val u1 = Tree.map2(Tree.map2(t1, t2)((_, _)), t3) { case ((x1, x2), x3) => (x1, x2, x3) }
        val u2 = Tree.map2(t1, Tree.map2(t2, t3)((_, _))) { case (x1, (x2, x3)) => (x1, x2, x3) }
        u1 == u2
    })
  }
}
