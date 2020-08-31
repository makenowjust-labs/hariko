package codes.quine.labo.gen.data

/**
  * Tree is [[https://en.wikipedia.org/wiki/Rose_tree rose tree]] implementation.
  *
  * In this library, it represents a pair of current value and its shrinked values.
  */
final case class Tree[T](value: T, children: LazyList[Tree[T]]) {
  def map[U](f: T => U): Tree[U] =
    Tree(f(value), children.map(_.map(f)))

  def productMap[U, V](other: Tree[U])(f: (T, U) => V): Tree[V] =
    Tree(
      f(value, other.value),
      children.map(other.productMap(_)((y, x) => f(x, y))) ++ other.children.map(productMap(_)(f))
    )

  def expand(f: T => Seq[T]): Tree[T] = {
    def fold(x: T): LazyList[Tree[T]] = LazyList.from(f(x)).map(x => Tree(x, fold(x)))
    Tree(value, fold(value) ++ children.map(_.expand(f)))
  }
}

object Tree {
  def pure[T](x: T): Tree[T] = Tree(x, LazyList.empty)
}
