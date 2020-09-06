package codes.quine.labo.hariko.data

/**
  * Tree is [[https://en.wikipedia.org/wiki/Rose_tree rose tree]] implementation.
  *
  * In this library, it represents a pair of current value and its shrunk values.
  */
final case class Tree[T](value: T, children: LazyList[Tree[T]]) {

  /** Converts each values by `f`. */
  def map[U](f: T => U): Tree[U] =
    Tree(f(value), children.map(_.map(f)))

  /**
    * Expands each values by `f`.
    *
    * Expanded values are prepended to its children.
    */
  def expand(f: T => Seq[T]): Tree[T] = {
    def forest(x: T): LazyList[Tree[T]] =
      LazyList(x).flatMap(f).map(x => Tree(x, forest(x))) ++ children.map(_.expand(f))
    Tree(value, forest(value))
  }
}

object Tree {

  /** Create a new [[Tree]] which has value `x` and no children. */
  def pure[T](x: T): Tree[T] = Tree(x, LazyList.empty)

  /** Returns two [[Tree]]s product with mapping. */
  def map2[T, U, V](tree1: Tree[T], tree2: Tree[U])(f: (T, U) => V): Tree[V] =
    Tree(
      f(tree1.value, tree2.value),
      tree1.children.map(map2(_, tree2)(f)) ++ tree2.children.map(map2(tree1, _)(f))
    )

  /**
    * Collapses option values in tree. If root value is None, result is also None.
    *
    * This signature looks like `Traverse.sequence` function, but its semantics are not relevant.
    */
  def collapse[T](tree: Tree[Option[T]]): Option[Tree[T]] =
    tree match {
      case Tree(Some(value), children) => Some(Tree(value, children.collect(Function.unlift(collapse(_)))))
      case _                           => None
    }
}
