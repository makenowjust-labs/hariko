package codes.quine.labo.gen
package data

import util.Shrink

sealed trait PartialFun[T, R] {

  def map[S](f: R => S): PartialFun[T, S]

  def table: LazyList[(T, R)]

  def toFunction(fallback: R): T => R

  def shrink(f: R => LazyList[R]): LazyList[PartialFun[T, R]]
}

object PartialFun {
  type :=>[T, R] = PartialFun[T, R]

  final case class Empty[T, R]() extends (T :=> R) {
    def map[S](f: R => S): T :=> S = Empty()

    def table: LazyList[(T, R)] = LazyList.empty

    def toFunction(fallback: R): T => R = _ => fallback

    def shrink(f: R => LazyList[R]): LazyList[T :=> R] = LazyList.empty
  }

  final case class Point[R](result: R) extends (Unit :=> R) {
    def map[S](f: R => S): Unit :=> S = Point(f(result))

    def table: LazyList[(Unit, R)] = LazyList(((), result))

    def toFunction(fallback: R): Unit => R = _ => result

    def shrink(f: R => LazyList[R]): LazyList[Unit :=> R] = f(result).map(Point(_))
  }

  final case class Uncurry[T1, T2, R](pfun: Lazy[T1 :=> (T2 :=> R)]) extends ((T1, T2) :=> R) {
    def map[S](f: R => S): (T1, T2) :=> S = Uncurry(Lazy(pfun.value.map(_.map(f))))

    def table: LazyList[((T1, T2), R)] =
      for {
        (x1, pfun2) <- pfun.value.table
        (x2, y) <- pfun2.table
      } yield ((x1, x2), y)

    def toFunction(fallback: R): ((T1, T2)) => R = {
      case (x1, x2) =>
        pfun.value.map(_.toFunction(fallback)(x2)).toFunction(fallback)(x1)
    }

    def shrink(f: R => LazyList[R]): LazyList[(T1, T2) :=> R] =
      pfun.value.shrink(_.shrink(f)).map {
        case Empty() => Empty()
        case pfun    => Uncurry(Lazy(pfun))
      }
  }

  final case class Choice[T1, T2, R](pfun1: Lazy[T1 :=> R], pfun2: Lazy[T2 :=> R]) extends (Either[T1, T2] :=> R) {
    def map[S](f: R => S): Either[T1, T2] :=> S = Choice(Lazy(pfun1.value.map(f)), Lazy(pfun2.value.map(f)))

    def table: LazyList[(Either[T1, T2], R)] =
      pfun1.value.table.map { case (x1, z) => (Left(x1), z) } ++ pfun2.value.table.map {
        case (x2, z) => (Right(x2), z)
      }

    def toFunction(fallback: R): Either[T1, T2] => R = {
      case Left(x1)  => pfun1.value.toFunction(fallback)(x1)
      case Right(x2) => pfun2.value.toFunction(fallback)(x2)
    }

    def shrink(f: R => LazyList[R]): LazyList[Either[T1, T2] :=> R] =
      (
        (if (!pfun1.value.isInstanceOf[Empty[T1, R]] && !pfun2.value.isInstanceOf[Empty[T2, R]])
           LazyList((Empty[T1, R](), pfun2.value), (pfun1.value, Empty[T2, R]()))
         else LazyList.empty) ++
          pfun1.value.shrink(f).map((_, pfun2.value)) ++
          pfun2.value.shrink(f).map((pfun1.value, _))
      ).map {
        case (Empty(), Empty()) => Empty()
        case (pfun1, pfun2)     => Choice(Lazy(pfun1), Lazy(pfun2))
      }
  }

  final case class Iso[T, U, R](forward: T => U, backward: U => T, pfun: Lazy[U :=> R]) extends (T :=> R) {
    def map[S](f: R => S): T :=> S = Iso(forward, backward, Lazy(pfun.value.map(f)))

    def table: LazyList[(T, R)] = pfun.value.table.map { case (y, z) => (backward(y), z) }

    def toFunction(fallback: R): T => R = x => pfun.value.toFunction(fallback)(forward(x))

    def shrink(f: R => LazyList[R]): LazyList[T :=> R] =
      pfun.value.shrink(f).map {
        case Empty() => Empty()
        case pfun    => Iso(forward, backward, Lazy(pfun))
      }
  }

  final case class Lift[T, R](domain: Set[T], f: T => R) extends (T :=> R) {
    def map[S](g: R => S): T :=> S = Lift(domain, f.andThen(g))

    def table: LazyList[(T, R)] = LazyList.from(domain).map(x => (x, f(x)))

    def toFunction(fallback: R): T => R = x => if (domain.contains(x)) f(x) else fallback

    def shrink(g: R => LazyList[R]): LazyList[T :=> R] = {
      Shrink.list(0, domain.toList).map(dom => if (dom.isEmpty) Empty[T, R]() else Lift(dom.toSet, f)) ++
        LazyList.from(domain).map(x => (x, f(x))).flatMap {
          case (x, y) => g(y).map(y => Lift(domain, Map(x -> y).withDefault(f)))
        }
    }
  }
}
