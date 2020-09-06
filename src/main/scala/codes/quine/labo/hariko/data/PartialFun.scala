package codes.quine.labo.hariko
package data

import scala.annotation.showAsInfix

/**
  * PartialFun is lazy data structure represents a partial function.
  *
  * Reference:
  *
  * - Claessen, Koen. (2012) "Shrinking and showing functions: (functional pearl)."
  *   [[https://doi.org/10.1145/2364506.2364516]]
  */
sealed abstract class PartialFun[T, R] extends Product with Serializable {

  /**
    * Converts result value by `f`.
    */
  def map[S](f: R => S): PartialFun[T, S]

  /**
    * Lists argument-result table.
    *
    * NOTE: this result is potentially infinite.
    */
  def table: LazyList[(T, R)]

  /**
    * Lifts up to usual function.
    */
  def lift: T => Option[R]
}

/**
  * PartialFun implementations.
  */
object PartialFun {

  /**
    * Operator notation of PartialFun.
    */
  @showAsInfix type :=>[T, R] = PartialFun[T, R]

  /**
    * An empty partial function.
    */
  final case class Empty[T, R]() extends (T :=> R) {
    def map[S](f: R => S): T :=> S = Empty()

    def table: LazyList[(T, R)] = LazyList.empty

    def lift: T => Option[R] = _ => None
  }

  /**
    * A partial function lifted down from usual function restricted by the domain.
    */
  final case class Unlift[T, R](domain: Set[T], f: T => R) extends (T :=> R) {
    def map[S](g: R => S): T :=> S = Unlift(domain, f.andThen(g))

    def table: LazyList[(T, R)] = LazyList.from(domain).map(x => (x, f(x)))

    def lift: T => Option[R] = x => if (domain.contains(x)) Some(f(x)) else None
  }

  /**
    * A curried partial function.
    */
  final case class Uncurry[T1, T2, R](pfun: T1 :=> (T2 :=> R)) extends ((T1, T2) :=> R) {
    def map[S](f: R => S): (T1, T2) :=> S = Uncurry(pfun.map(_.map(f)))

    def table: LazyList[((T1, T2), R)] =
      for {
        (x1, pfun2) <- pfun.table
        (x2, y) <- pfun2.table
      } yield ((x1, x2), y)

    def lift: ((T1, T2)) => Option[R] = {
      case (x1, x2) =>
        pfun.map(_.lift(x2)).lift(x1).flatten
    }
  }

  /**
    * A partial function accepts either values.
    */
  final case class Choice[T1, T2, R](pfun1: T1 :=> R, pfun2: T2 :=> R) extends (Either[T1, T2] :=> R) {
    def map[S](f: R => S): Either[T1, T2] :=> S = Choice(pfun1.map(f), pfun2.map(f))

    def table: LazyList[(Either[T1, T2], R)] =
      pfun1.table.map { case (x1, z) => (Left(x1), z) } ++ pfun2.table.map { case (x2, z) => (Right(x2), z) }

    def lift: Either[T1, T2] => Option[R] = {
      case Left(x1)  => pfun1.lift(x1)
      case Right(x2) => pfun2.lift(x2)
    }
  }

  /**
    * A partial function converted by providing both sides transformation.
    */
  final case class Iso[T, U, R](forward: T => U, backward: U => T, pfun: U :=> R) extends (T :=> R) {
    def map[S](f: R => S): T :=> S = Iso(forward, backward, pfun.map(f))

    def table: LazyList[(T, R)] = pfun.table.map { case (y, z) => (backward(y), z) }

    def lift: T => Option[R] = x => pfun.lift(forward(x))
  }
}
