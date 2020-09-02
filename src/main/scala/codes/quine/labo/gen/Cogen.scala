package codes.quine.labo.gen

import scala.annotation.tailrec

import data.PartialFun._
import random.Random
import util.Bytes
import util.Shrink
import data.Tree
import data.LocalFun

/** Cogen is dual of [[Gen]], which is used for building function [[Gen]]s. */
trait Cogen[T] { cogen =>

  def build[R](gen: Gen[R]): Gen[T :=> Option[R]]

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
    * Creates a [[data.PartialFun PartialFun]] generator via [[Lift]].
    *
    * @group util
    */
  def lift[T, R](domain: Set[T], gen: Gen[R])(f: (T, Random) => Random): Gen[T :=> Option[R]] =
    Gen { (rand0, param, scale) =>
      val (rand1, rand2) = rand0.split
      val func: T => Option[Tree[R]] = x => {
        val rand3 = f(x, rand2)
        gen.unsafeGet(rand3, param, scale).map(_._2)
      }
      val pfun: T :=> Option[Tree[R]] = Lift(domain, func)
      val t = Tree.pure(pfun).expand(Shrink.partialFun)
      (rand1, t.map(pfun => Some(pfun.map(_.map(_.value)))))
    }

  /**
    * Returns a unit [[Cogen]].
    *
    * @group primitive
    */
  def unit: Cogen[Unit] =
    new Cogen[Unit] {
      def build[R](gen: Gen[R]): Gen[Unit :=> Option[R]] =
        lift(Set(()), gen)((_, rand) => rand)
    }

  /**
    * Returns a boolean [[Cogen]].
    *
    * @group primitive
    */
  def boolean: Cogen[Boolean] =
    new Cogen[Boolean] {
      def build[R](gen: Gen[R]): Gen[Boolean :=> Option[R]] =
        lift(Set(true, false), gen)((x, rand) => if (x) rand.right else rand.left)
    }

  /**
    * Returns a byte [[Cogen]].
    *
    * @group primitive
    */
  def byte: Cogen[Byte] =
    new Cogen[Byte] {
      @tailrec def variant(x: Byte, rand: Random): Random =
        if (x == 0) rand.left
        else variant(((x & 0xff) >>> 1).toByte, (if ((x & 1) == 0) rand.left else rand.right).right)
      def build[R](gen: Gen[R]): Gen[Byte :=> Option[R]] =
        lift(Set.from(-128 to 127).map(_.toByte), gen)(variant)
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

  /**
    * Returns a long [[Cogen]].
    *
    * @group primitive
    */
  def long: Cogen[Long] =
    tuple2(int, int).imap(Bytes.compose(_))(Bytes.decompose(_))

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
    * Returns an either [[Cogen]].
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
}
