package codes.quine.labo.hariko

import scala.annotation.tailrec

import data.PartialFun._
import random.Random
import util.Bytes
import util.Shrink
import data.Tree
import data.LocalFun
import data.UntupledFun._

/**
  * Cogen provides an ability to build function generator from result value generator.
  *
  * It is lawful as `Invariant` or `InvariantMonoidal`.
  */
trait Cogen[T] { cogen =>

  /**
    * Builds a [[data.PartialFun PartialFun]] generator from result value generator.
    */
  def build[R](gen: Gen[R]): Gen[T :=> Option[R]]

  /**
    * Converts this cogen from `T` to `U` by providing both sides transformation.
    */
  def imap[U](f: T => U)(g: U => T): Cogen[U] =
    new Cogen[U] {
      def build[R](gen: Gen[R]): Gen[U :=> Option[R]] =
        cogen.build(gen).map {
          case Empty() => Empty()
          case pfun    => Iso(g, f, pfun)
        }
    }
}

/**
  * Utilities for cogen.
  *
  * @groupname util Utility Functions
  * @groupprio util 0
  *
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
    * Creates a new cogen from `domain` and `variant` function.
    *
    * Given values are used for:
    *
    *   - `domain` is a set contains all values of `T`.
    *   - `variant` is function to update PRNG state by the value.
    *
    * @group util
    */
  def unlift[T](domain: Set[T])(variant: (T, Random) => Random): Cogen[T] =
    new Cogen[T] {
      def build[R](gen: Gen[R]): Gen[T :=> Option[R]] =
        Gen { (rand0, param, scale) =>
          val (rand1, rand2) = rand0.split
          val func: T => Option[Tree[R]] = x => {
            val rand3 = variant(x, rand2)
            gen.unsafeHead(rand3, param, scale).map(_._2)
          }
          val pfun: T :=> Option[Tree[R]] = Unlift(domain, func)
          val t = Tree.pure(pfun).expand(Shrink.partialFun)
          (rand1, t.map(pfun => Some(pfun.map(_.map(_.value)))))
        }
    }

  /**
    * A unit cogen.
    *
    * @group primitive
    */
  def unit: Cogen[Unit] =
    unlift(Set(()))((_, rand) => rand)

  /**
    * A boolean cogen.
    *
    * @group primitive
    */
  def boolean: Cogen[Boolean] =
    unlift(Set(true, false))((x, rand) => if (x) rand.right else rand.left)

  /**
    * A byte cogen.
    *
    * @group primitive
    */
  def byte: Cogen[Byte] = {
    @tailrec def variant(x: Byte, rand: Random): Random =
      if (x == 0) rand.left
      else variant(((x & 0xff) >>> 1).toByte, (if ((x & 1) == 0) rand.left else rand.right).right)
    unlift(Set.from(-128 to 127).map(_.toByte))(variant)
  }

  /**
    * A short cogen.
    *
    * @group primitive
    */
  def short: Cogen[Short] =
    tuple2(byte, byte).imap(Bytes.compose(_))(Bytes.decompose(_))

  /**
    * An int cogen.
    *
    * @group primitive
    */
  def int: Cogen[Int] =
    tuple4(byte, byte, byte, byte).imap(Bytes.compose(_))(Bytes.decompose(_))

  /**
    * A long cogen.
    *
    * @group primitive
    */
  def long: Cogen[Long] =
    tuple2(int, int).imap(Bytes.compose(_))(Bytes.decompose(_))

  /**
    * A char cogen.
    *
    * @group primitive
    */
  def char: Cogen[Char] = short.imap(_.toChar)(_.toShort)

  /**
    * A float cogen.
    *
    * @group primitive
    */
  def float: Cogen[Float] =
    int.imap(java.lang.Float.intBitsToFloat(_))(java.lang.Float.floatToIntBits(_))

  /**
    * A double cogen.
    *
    * @group primitive
    */
  def double: Cogen[Double] =
    long.imap(java.lang.Double.longBitsToDouble(_))(java.lang.Double.doubleToLongBits(_))

  /**
    * A string cogen.
    *
    * @group collection
    */
  def string: Cogen[String] = list(char).imap(_.mkString)(_.toCharArray().toList)

  /**
    * A list cogen.
    *
    * @group collection
    */
  def list[T](cogen: Cogen[T]): Cogen[List[T]] =
    new Cogen[List[T]] { list =>
      val instance = either(unit, tuple2(cogen, list))
        .imap(backward)(forward)

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

      def build[R](gen: Gen[R]): Gen[List[T] :=> Option[R]] =
        instance.build(gen)
    }

  /**
    * An option cogen.
    *
    * @group collection
    */
  def option[T](cogen: Cogen[T]): Cogen[Option[T]] =
    either(unit, cogen).imap(_.fold(_ => None, Some(_)))(_.fold(Left(()): Either[Unit, T])(Right(_)))

  /**
    * An either cogen.
    *
    * @group collection
    */
  def either[T, U](leftCogen: Cogen[T], rightCogen: Cogen[U]): Cogen[Either[T, U]] =
    new Cogen[Either[T, U]] {
      def build[R](gen: Gen[R]): Gen[Either[T, U] :=> Option[R]] =
        Gen.delay {
          val leftGen = leftCogen.build(gen)
          val rightGen = rightCogen.build(gen)
          Gen.map2(leftGen, rightGen) {
            case (Empty(), Empty()) => Empty()
            case (l, r)             => Choice(l, r)
          }
        }
    }

  /**
    * A two elements tuple cogen.
    *
    * @group collection
    */
  def tuple2[T1, T2](cogen1: Cogen[T1], cogen2: Cogen[T2]): Cogen[(T1, T2)] =
    new Cogen[(T1, T2)] {
      def build[R](gen: Gen[R]): Gen[(T1, T2) :=> Option[R]] =
        Gen.delay {
          val gen2 = cogen2.build(gen)
          val gen1 = cogen1.build(gen2)
          gen1.map {
            case Empty() => Empty()
            case pfun1 =>
              Uncurry(pfun1.map {
                case None        => Empty()
                case Some(pfun2) => pfun2
              })
          }
        }
    }

  /**
    * A three elements tuple cogen.
    *
    * @group collection
    */
  def tuple3[T1, T2, T3](cogen1: Cogen[T1], cogen2: Cogen[T2], cogen3: Cogen[T3]): Cogen[(T1, T2, T3)] =
    tuple2(tuple2(cogen1, cogen2), cogen3).imap { case ((x1, x2), x3) => (x1, x2, x3) } {
      case (x1, x2, x3) => ((x1, x2), x3)
    }

  /**
    * A four elements tuple cogen.
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
    * A function cogen.
    *
    * @group function
    */
  def function1[T1, R](gen1: Gen[T1], cogen: Cogen[R]): Cogen[T1 => R] =
    new Cogen[T1 => R] {
      def build[S](gen: Gen[S]): Gen[(T1 => R) :=> Option[S]] =
        Gen.delay {
          val pfunGen = cogen.build(gen)
          Gen.map2(gen1, pfunGen) {
            case (_, Empty()) => Empty()
            case (x, pfun)    => Iso(f => f(x), (y: R) => new LocalFun(x, y), pfun)
          }
        }
    }

  /**
    * A two inputs cogen.
    *
    * @group function
    */
  def function2[T1, T2, R](gen1: Gen[T1], gen2: Gen[T2], cogen: Cogen[R]): Cogen[(T1, T2) => R] =
    function1(Gen.tuple2(gen1, gen2), cogen).imap(new UntupledFun2(_).asInstanceOf[(T1, T2) => R])(_.tupled)

  /**
    * A three inputs cogen.
    *
    * @group function
    */
  def function3[T1, T2, T3, R](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], cogen: Cogen[R]): Cogen[(T1, T2, T3) => R] =
    function1(Gen.tuple3(gen1, gen2, gen3), cogen).imap(new UntupledFun3(_).asInstanceOf[(T1, T2, T3) => R])(_.tupled)

  /**
    * A four inputs cogen.
    *
    * @group function
    */
  def function4[T1, T2, T3, T4, R](
      gen1: Gen[T1],
      gen2: Gen[T2],
      gen3: Gen[T3],
      gen4: Gen[T4],
      cogen: Cogen[R]
  ): Cogen[(T1, T2, T3, T4) => R] =
    function1(Gen.tuple4(gen1, gen2, gen3, gen4), cogen)
      .imap(new UntupledFun4(_).asInstanceOf[(T1, T2, T3, T4) => R])(_.tupled)
}
