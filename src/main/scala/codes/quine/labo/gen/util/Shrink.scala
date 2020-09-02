package codes.quine.labo.gen
package util

object Shrink {
  def long(base: Long, x: Long): LazyList[Long] =
    if (x == base) LazyList.empty
    else if (Math.abs(x - base) == 1) LazyList(base)
    else LazyList.iterate(x / 2 - base / 2)(_ / 2).takeWhile(_ != 0).map(x - _)

  def list[T](minSize: Int, xs: List[T]): LazyList[List[T]] = {
    def interleave[T](xs: LazyList[T], ys: LazyList[T]): LazyList[T] =
      if (xs.isEmpty) ys
      else if (ys.isEmpty) xs
      else LazyList.cons(xs.head, interleave(ys, xs.tail))

    def loop(xs: List[T]): LazyList[List[T]] =
      xs.size match {
        case 0 => LazyList.empty
        case 1 => LazyList(List.empty)
        case n =>
          val (xs0, xs1) = xs.splitAt(n / 2)
          lazy val rest =
            interleave(loop(xs1).filter(_.nonEmpty).map(xs0 ++ _), loop(xs0).filter(_.nonEmpty).map(_ ++ xs1))
          LazyList.cons(xs0, LazyList.cons(xs1, rest))
      }

    loop(xs).dropWhile(_.size < minSize)
  }
}
