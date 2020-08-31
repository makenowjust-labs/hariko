package codes.quine.labo.gen

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

import data.Tree
import random.Random

/** Cogen is dual of [[Gen]], which is used for building function [[Gen]]s. */
trait Cogen[T] { cogen =>

  /** Modifies `rand` by `x` value. */
  def variant(x: T, rand: Random, scale: Int): Random

  /** Applies `f` before `variant`. */
  def contramap[U](f: U => T): Cogen[U] =
    new Cogen[U] {
      def variant(y: U, rand: Random, scale: Int): Random = cogen.variant(f(y), rand, scale)
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
    }

  /**
    * Returns a byte [[Cogen]].
    *
    * @group primitive
    */
  def byte: Cogen[Byte] = long.contramap(_.toLong)

  /**
    * Returns a short [[Cogen]].
    *
    * @group primitive
    */
  def short: Cogen[Short] = long.contramap(_.toLong)

  /**
    * Returns an int [[Cogen]].
    *
    * @group primitive
    */
  def int: Cogen[Int] = long.contramap(_.toLong)

  /**
    * Returns a long [[Cogen]].
    *
    * @group primitive
    */
  def long: Cogen[Long] =
    new Cogen[Long] {
      @tailrec def variant(x: Long, rand: Random, scale: Int): Random =
        if (x == 0) rand.left
        else variant(x >> 1, (if ((x & 1) == 0) rand.left else rand.right).right, scale)
    }

  /**
    * Returns a char [[Cogen]].
    *
    * @group primitive
    */
  def char: Cogen[Char] = long.contramap(_.toLong)

  /**
    * Returns a string [[Cogen]].
    *
    * @group collection
    */
  def string: Cogen[String] = list(char).contramap(_.toCharArray().toList)

  /**
    * Returns a list [[Cogen]].
    *
    * @group collection
    */
  def list[T](cogen: Cogen[T]): Cogen[List[T]] =
    new Cogen[List[T]] {
      @tailrec def variant(xs: List[T], rand: Random, scale: Int): Random =
        xs match {
          case Nil     => rand.left
          case x :: xs => variant(xs, cogen.variant(x, rand.right, scale), scale)
        }
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
    }

  /**
    * Returns an option [[Cogen]].
    *
    * @group collection
    */
  def option[T](cogen: Cogen[T]): Cogen[Option[T]] =
    new Cogen[Option[T]] {
      def variant(x: Option[T], rand: Random, scale: Int): Random =
        x match {
          case None    => rand.left
          case Some(a) => cogen.variant(a, rand.right, scale)
        }
    }

  /**
    * Returns a two elements tuple [[Cogen]].
    *
    * @group collection
    */
  def tuple2[T1, T2](cogen1: Cogen[T1], cogen2: Cogen[T2]): Cogen[(T1, T2)] =
    new Cogen[(T1, T2)] {
      def variant(xy: (T1, T2), rand: Random, scale: Int): Random =
        cogen1.variant(xy._1, rand, scale).pipe(cogen2.variant(xy._2, _, scale))
    }

  /**
    * Returns a three elements tuple [[Cogen]].
    *
    * @group collection
    */
  def tuple3[T1, T2, T3](cogen1: Cogen[T1], cogen2: Cogen[T2], cogen3: Cogen[T3]): Cogen[(T1, T2, T3)] =
    tuple2(tuple2(cogen1, cogen2), cogen3).contramap { case (x1, x2, x3) => ((x1, x2), x3) }

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
    tuple2(tuple3(cogen1, cogen2, cogen3), cogen4).contramap { case (x1, x2, x3, x4) => ((x1, x2, x3), x4) }

  /**
    * Returns a function [[Cogen]].
    *
    * @group function
    */
  def function1[T1, V](gen1: Gen[T1], cogen: Cogen[V]): Cogen[T1 => V] =
    new Cogen[T1 => V] {
      def variant(f: T1 => V, rand0: Random, scale: Int): Random = {
        // TODO: the following line is very unsafe, but how do we get generated value in safe?
        val (rand1, x) = LazyList
          .iterate(gen1.run(rand0, scale)) { case (rand, _) => gen1.run(rand, scale) }
          .collect { case (rand, Tree(Some(x), _)) => (rand, x) }
          .head
        cogen.variant(f(x), rand1, scale)
      }
    }

  /**
    * Returns a two inputs function [[Cogen]].
    *
    * @group function
    */
  def function2[T1, T2, V](gen1: Gen[T1], gen2: Gen[T2], cogen: Cogen[V]): Cogen[(T1, T2) => V] =
    function1(Gen.tuple2(gen1, gen2), cogen).contramap(_.tupled)

  /**
    * Returns a three inputs function [[Cogen]].
    *
    * @group function
    */
  def function3[T1, T2, T3, V](gen1: Gen[T1], gen2: Gen[T2], cogen: Cogen[V]): Cogen[(T1, T2) => V] =
    function1(Gen.tuple2(gen1, gen2), cogen).contramap(_.tupled)

  /**
    * Returns a four inputs function [[Cogen]].
    *
    * @group function
    */
  def function4[T1, T2, V](gen1: Gen[T1], gen2: Gen[T2], cogen: Cogen[V]): Cogen[(T1, T2) => V] =
    function1(Gen.tuple2(gen1, gen2), cogen).contramap(_.tupled)
}
