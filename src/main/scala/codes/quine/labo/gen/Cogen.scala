package codes.quine.labo.gen

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

import data.Fun
import data.Lazy
import data.PartialFun._
import random.Random
import util.Bytes

/** Cogen is dual of [[Gen]], which is used for building function [[Gen]]s. */
trait Cogen[T] { cogen =>

  /** Modifies `rand` by `x` value. */
  def variant(x: T, rand: Random, scale: Int): Random

  def build[R](f: T => R): T :=> R

  /** Applies `f` before `variant`. */
  def imap[U](f: T => U)(g: U => T): Cogen[U] =
    new Cogen[U] {
      def variant(y: U, rand: Random, scale: Int): Random = cogen.variant(g(y), rand, scale)
      def build[R](h: U => R): U :=> R = Iso(g, f, Lazy(cogen.build(f.andThen(h))))
    }
}

/**
  * @groupname primitive Primitive Co-generators
  * @groupprio primitive 200
  *
  * @groupname collection Collection Co-generators
  * @groupprio collection 210
  *
  * @groupname function Function Co-generators
  * @groupprio function 220
  */
object Cogen {

  /**
    * Returns a unit [[Cogen]].
    *
    * @group primitive
    */
  def unit: Cogen[Unit] =
    new Cogen[Unit] {
      def variant(x: Unit, rand: Random, scale: Int): Random = rand
      def build[R](f: Unit => R): Unit :=> R = Point(f(()))
    }

  /**
    * Returns a boolean [[Cogen]].
    *
    * @group primitive
    */
  def boolean: Cogen[Boolean] =
    new Cogen[Boolean] {
      def variant(x: Boolean, rand: Random, scale: Int): Random =
        if (x) rand.right else rand.left
      def build[R](f: Boolean => R): Boolean :=> R = Lift(Set(true, false), f)
    }

  /**
    * Returns a byte [[Cogen]].
    *
    * @group primitive
    */
  def byte: Cogen[Byte] =
    new Cogen[Byte] {
      @tailrec def variant(x: Byte, rand: Random, scale: Int): Random =
        if (x == 0) rand.left
        else variant((x >> 1).toByte, (if ((x & 1) == 0) rand.left else rand.right).right, scale)
      def build[R](f: Byte => R): Byte :=> R = Lift(Set.from((-128 to 127)).map(_.toByte), f)
    }

  /**
    * Returns a short [[Cogen]].
    *
    * @group primitive
    */
  def short: Cogen[Short] =
    tuple2(byte, byte).imap(Bytes.compose(_))(Bytes.decompose(_))

  /**
    * Returns an int [[Cogen]].
    *
    * @group primitive
    */
  def int: Cogen[Int] =
    tuple4(byte, byte, byte, byte).imap(Bytes.compose(_))(Bytes.decompose(_))

  // TODO: define long [[Cogen]] instance.

  /**
    * Returns a char [[Cogen]].
    *
    * @group primitive
    */
  def char: Cogen[Char] = short.imap(_.toChar)(_.toShort)

  /**
    * Returns a string [[Cogen]].
    *
    * @group collection
    */
  def string: Cogen[String] = list(char).imap(_.mkString)(_.toCharArray().toList)

  /**
    * Returns a list [[Cogen]].
    *
    * @group collection
    */
  def list[T](cogen: Cogen[T]): Cogen[List[T]] =
    new Cogen[List[T]] { list =>
      val instance = either(unit, tuple2(cogen, list))
        .imap(backward)(forward)

      @tailrec def variant(xs: List[T], rand: Random, scale: Int): Random =
        xs match {
          case Nil     => rand.left
          case x :: xs => variant(xs, cogen.variant(x, rand.right, scale), scale)
        }

      def forward(x: List[T]): Either[Unit, (T, List[T])] =
        x match {
          case Nil     => Left(())
          case x :: xs => Right((x, xs))
        }

      def backward(x: Either[Unit, (T, List[T])]): List[T] =
        x match {
          case Left(())       => Nil
          case Right((x, xs)) => x :: xs
        }

      def build[R](f: List[T] => R): List[T] :=> R =
        instance.build(f)
    }

  /**
    * Returns an either [[Cogen]].
    *
    * @group collection
    */
  def either[T, U](leftCogen: Cogen[T], rightCogen: Cogen[U]): Cogen[Either[T, U]] =
    new Cogen[Either[T, U]] {
      def variant(x: Either[T, U], rand: Random, scale: Int): Random =
        x match {
          case Left(a)  => leftCogen.variant(a, rand.left, scale)
          case Right(b) => rightCogen.variant(b, rand.right, scale)
        }
      def build[R](f: Either[T, U] => R): Either[T, U] :=> R =
        Choice(Lazy(leftCogen.build(a => f(Left(a)))), Lazy(rightCogen.build(b => f(Right(b)))))
    }

  /**
    * Returns an option [[Cogen]].
    *
    * @group collection
    */
  def option[T](cogen: Cogen[T]): Cogen[Option[T]] =
    either(unit, cogen).imap(_.fold(_ => None, Some(_)))(_.fold(Left(()): Either[Unit, T])(Right(_)))

  /**
    * Returns a two elements tuple [[Cogen]].
    *
    * @group collection
    */
  def tuple2[T1, T2](cogen1: Cogen[T1], cogen2: Cogen[T2]): Cogen[(T1, T2)] =
    new Cogen[(T1, T2)] {
      def variant(xy: (T1, T2), rand: Random, scale: Int): Random =
        cogen1.variant(xy._1, rand, scale).pipe(cogen2.variant(xy._2, _, scale))
      def build[R](f: ((T1, T2)) => R): (T1, T2) :=> R =
        Uncurry(Lazy(cogen1.build(x1 => cogen2.build(x2 => f((x1, x2))))))
    }

  /**
    * Returns a three elements tuple [[Cogen]].
    *
    * @group collection
    */
  def tuple3[T1, T2, T3](cogen1: Cogen[T1], cogen2: Cogen[T2], cogen3: Cogen[T3]): Cogen[(T1, T2, T3)] =
    tuple2(tuple2(cogen1, cogen2), cogen3).imap { case ((x1, x2), x3) => (x1, x2, x3) } {
      case (x1, x2, x3) => ((x1, x2), x3)
    }

  /**
    * Returns a four elements tuple [[Cogen]].
    *
    * @group collection
    */
  def tuple4[T1, T2, T3, T4](
      cogen1: Cogen[T1],
      cogen2: Cogen[T2],
      cogen3: Cogen[T3],
      cogen4: Cogen[T4]
  ): Cogen[(T1, T2, T3, T4)] =
    tuple2(tuple3(cogen1, cogen2, cogen3), cogen4).imap { case ((x1, x2, x3), x4) => (x1, x2, x3, x4) } {
      case (x1, x2, x3, x4) => ((x1, x2, x3), x4)
    }

  /**
    * Returns a function [[Cogen]].
    *
    * @group function
    */
  def function1[T1, R](gen1: Gen[T1], cogen: Cogen[R]): Cogen[T1 => R] =
    new Cogen[T1 => R] {
      def variant(f: T1 => R, rand0: Random, scale: Int): Random =
        gen1.unsafeRun(rand0, scale) match {
          case Some((rand1, t)) => cogen.variant(f(t.value), rand1, scale)
          case None =>
            f match {
              case fun: Fun[T1, R] => cogen.variant(fun.fallback, rand0, scale)
              case _               => rand0
            }
        }
      // TODO: support shrinking higher order function (is it possible?)
      def build[S](f: (T1 => R) => S): (T1 => R) :=> S = Empty[T1 => R, S]()
    }

  /**
    * Returns a two inputs function [[Cogen]].
    *
    * @group function
    */
  def function2[T1, T2, V](gen1: Gen[T1], gen2: Gen[T2], cogen: Cogen[V]): Cogen[(T1, T2) => V] =
    function1(Gen.tuple2(gen1, gen2), cogen).imap(Function.untupled(_))(_.tupled)

  /**
    * Returns a three inputs function [[Cogen]].
    *
    * @group function
    */
  def function3[T1, T2, T3, V](gen1: Gen[T1], gen2: Gen[T2], cogen: Cogen[V]): Cogen[(T1, T2) => V] =
    function1(Gen.tuple2(gen1, gen2), cogen).imap(Function.untupled(_))(_.tupled)

  /**
    * Returns a four inputs function [[Cogen]].
    *
    * @group function
    */
  def function4[T1, T2, V](gen1: Gen[T1], gen2: Gen[T2], cogen: Cogen[V]): Cogen[(T1, T2) => V] =
    function1(Gen.tuple2(gen1, gen2), cogen).imap(Function.untupled(_))(_.tupled)
}
