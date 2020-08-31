package codes.quine.labo.gen

import scala.annotation.tailrec

import data.Tree
import random.Random

trait Cogen[T] { cogen =>
  def variant[U](x: T, gen: Gen[U]): Gen[U]

  def contramap[U](f: U => T): Cogen[U] =
    new Cogen[U] {
      def variant[V](y: U, gen: Gen[V]): Gen[V] = cogen.variant(f(y), gen)
    }
}

object Cogen {
  def boolean: Cogen[Boolean] =
    new Cogen[Boolean] {
      def variant[U](x: Boolean, gen: Gen[U]): Gen[U] =
        Gen { (rand0, scale) =>
          val rand1 = if (x) rand0.right else rand0.left
          gen.run(rand1, scale)
        }
    }

  def byte: Cogen[Byte] = long.contramap(_.toLong)
  def short: Cogen[Short] = long.contramap(_.toLong)
  def int: Cogen[Int] = long.contramap(_.toLong)

  def long: Cogen[Long] =
    new Cogen[Long] {
      def variant[U](x: Long, gen: Gen[U]): Gen[U] =
        Gen { (rand0, scale) =>
          @tailrec def loop(x: Long, rand: Random): Random =
            if (x == 0) rand.left
            else loop(x >> 1, (if ((x & 1) == 0) rand.left else rand.right).right)
          val rand1 = loop(x, rand0)
          gen.run(rand1, scale)
        }
    }

  def char: Cogen[Char] = long.contramap(_.toLong)

  def either[T, U](leftCogen: Cogen[T], rightCogen: Cogen[U]): Cogen[Either[T, U]] =
    new Cogen[Either[T, U]] {
      def variant[V](x: Either[T, U], gen0: Gen[V]): Gen[V] =
        Gen { (rand0, scale) =>
          val (rand1, gen1) = x match {
            case Left(a)  => (rand0.left, leftCogen.variant(a, gen0))
            case Right(b) => (rand0.right, rightCogen.variant(b, gen0))
          }
          gen1.run(rand1, scale)
        }
    }

  def option[T](cogen: Cogen[T]): Cogen[Option[T]] =
    new Cogen[Option[T]] {
      def variant[U](x: Option[T], gen0: Gen[U]): Gen[U] =
        Gen { (rand0, scale) =>
          val (rand1, gen1) = x match {
            case None    => (rand0.left, gen0)
            case Some(a) => (rand0.right, cogen.variant(a, gen0))
          }
          gen1.run(rand1, scale)
        }
    }

  def tuple2[T1, T2](cogen1: Cogen[T1], cogen2: Cogen[T2]): Cogen[(T1, T2)] =
    new Cogen[(T1, T2)] {
      def variant[U](xy: (T1, T2), gen0: Gen[U]): Gen[U] =
        Gen { (rand0, scale) =>
          val gen1 = cogen1.variant(xy._1, gen0)
          val gen2 = cogen2.variant(xy._2, gen1)
          gen2.run(rand0, scale)
        }
    }

  def tuple3[T1, T2, T3](cogen1: Cogen[T1], cogen2: Cogen[T2], cogen3: Cogen[T3]): Cogen[(T1, T2, T3)] =
    tuple2(tuple2(cogen1, cogen2), cogen3).contramap { case (x1, x2, x3) => ((x1, x2), x3) }

  def tuple4[T1, T2, T3, T4](
      cogen1: Cogen[T1],
      cogen2: Cogen[T2],
      cogen3: Cogen[T3],
      cogen4: Cogen[T4]
  ): Cogen[(T1, T2, T3, T4)] =
    tuple2(tuple3(cogen1, cogen2, cogen3), cogen4).contramap { case (x1, x2, x3, x4) => ((x1, x2, x3), x4) }

  def function1[T1, V](argGen1: Gen[T1], cogen: Cogen[V]): Cogen[T1 => V] =
    new Cogen[T1 => V] {
      def variant[U](f: T1 => V, gen0: Gen[U]): Gen[U] =
        Gen { (rand0, scale) =>
          // TODO: the following line is very unsafe, but how do we get generated value in safe?
          val (rand1, x) = LazyList
            .iterate(argGen1.run(rand0, scale)) { case (rand, _) => argGen1.run(rand, scale) }
            .collect { case (rand, Tree(Some(x), _)) => (rand, x) }
            .head
          val gen1 = cogen.variant(f(x), gen0)
          gen1.run(rand1, scale)
        }
    }
}
