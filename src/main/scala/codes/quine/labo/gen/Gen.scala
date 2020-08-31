package codes.quine.labo.gen

import data.Range
import data.Tree
import random.Random
import util.Shrink

/**
 * Gen is random value generator in scale with shrinking.
 *
 * Techenically, this type is isomorphic to `cats` type `ReaderT[Int, Nested[State[Random, *], Nested[Tree, Option, *], *], *]`.
 * Thus it is lawful as `Functor`, `Applicative` or `FunctorFilter`.
 */
final case class Gen[T](run: (Random, Int) => (Random, Tree[Option[T]])) {
  /**
   * Generates values with the given parameter.
   *
   * It is for testing or demonstrating [[Gen]] behavior.
   *
   * {{{
   * scale> Gen.int(Range.linear(0, -100, 100)).samples().take(10).toList
   * val res0: List[Int] = List(-48, -68, -73, -9, 4, -78, 74, -51, 76, 99)
   * }}}
   */
  def samples(rand: Random = Random(42), scale: Int = 100): LazyList[T] =
    LazyList.iterate((rand, Option.empty[T])) { case (rand0, _) =>
      val (rand1, t) = run(rand0, scale)
      (rand1, t.value)
    }.collect { case (_, Some(x)) => x }

  def map[U](f: T => U): Gen[U] =
    Gen { (rand0, scale) =>
      val (rand1, t) = run(rand0, scale)
      (rand1, t.map(_.map(f)))
    }

  def productMap[U, V](other: Gen[U])(f: (T, U) => V): Gen[V] =
    Gen { (rand0, scale) =>
      val (rand1, t) = run(rand0, scale)
      val (rand2, u) = other.run(rand1, scale)
      (rand2, t.productMap(u)((a, b) => for (x <- a; y <- b) yield f(x, y)))
    }

  def filter(f: T => Boolean): Gen[T] =
    Gen { (rand0, scale) =>
      val (rand1, t) = run(rand0, scale)
      (rand1, t.map(_.filter(f)))
    }

  def withFilter(f: T => Boolean): Gen[T] = filter(f)
}

object Gen {
  def delay[T](gen: => Gen[T]): Gen[T] = {
    lazy val gen1 = gen
    Gen((rand, scale) => gen1.run(rand, scale))
  }

  def pure[T](x: T): Gen[T] = Gen((rand, _) => (rand, Tree.pure(Some(x))))

  def map2[T1, T2, U](gen1: Gen[T1], gen2: Gen[T2])(f: (T1, T2) => U): Gen[U] =
    gen1.productMap(gen2)(f)

  def map3[T1, T2, T3, U](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3])(f: (T1, T2, T3) => U): Gen[U] =
    tuple2(gen1, gen2).productMap(gen3) { case ((x1, x2), x3) => f(x1, x2, x3) }

  def map4[T1, T2, T3, T4, U](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4])(f: (T1, T2, T3, T4) => U): Gen[U] =
    tuple3(gen1, gen2, gen3).productMap(gen4) { case ((x1, x2, x3), x4) => f(x1, x2, x3, x4) }

  def replicate[T](n: Int, gen: Gen[T]): Gen[List[T]] = {
    require(n >= 0, "gen.Gen.replicate: invalid replicate number")
    if (n == 0) Gen.pure(List.empty)
    else if (n == 1) gen.map(List(_))
    else if (n % 2 == 1) gen.productMap(replicate(n - 1, gen))(_ +: _)
    else {
      val gen2 = replicate(n / 2, gen)
      gen2.productMap(gen2)(_ ++ _)
    }
  }

  def setReplicate[T](n: Int, gen: Gen[T]): Gen[List[T]] = {
    require(n >= 0, "gen.Gen.setReplicate: invalid replicate number")
    if (n == 0) Gen.pure(List.empty)
    else if (n == 1) gen.map(List(_))
    else if (n % 2 == 1)
      tuple2(gen, setReplicate(n - 1, gen)).filter { case (x, xs) => !xs.contains(x) }.map { case (x, xs) => x :: xs }
    else {
      val gen2 = setReplicate(n / 2, gen)
      tuple2(gen2, gen2).filter { case (xs1, xs2) => xs1.forall(!xs2.contains(_)) }.map { case (xs1, xs2) => xs1 ++ xs2 }
    }
  }

  def byte(range: Range[Byte]): Gen[Byte] = long(range.map(_.toLong)).map(_.toByte)
  def short(range: Range[Short]): Gen[Short] = long(range.map(_.toLong)).map(_.toShort)
  def int(range: Range[Int]): Gen[Int] = long(range.map(_.toLong)).map(_.toInt)

  def long(range: Range[Long]): Gen[Long] =
    Gen { (rand0, scale) =>
      val (rand1, x) = rand0.nextLong(range.bounds(scale))
      val t = Tree.pure(x).expand(Shrink.long(range.base, _))
      (rand1, t.map(Some(_)))
    }

  def listOf[T](gen: Gen[T], sizeRange: Range[Int]): Gen[List[T]] =
    Gen { (rand0, scale) =>
      val bounds@(min, _) = sizeRange.map(_.toLong).bounds(scale)
      val (rand1, n) = rand0.nextLong(bounds)
      val (rand2, t0) = replicate(n.toInt, gen).run(rand1, scale)
      val t1 = t0.expand {
        case None => Seq.empty
        case Some(xs) => Shrink.list(min.toInt, xs).map(Some(_))
      }
      (rand2, t1)
    }

  def setOf[T](gen: Gen[T], sizeRange: Range[Int]): Gen[Set[T]] =
    Gen { (rand0, scale) =>
      val bounds@(min, _) = sizeRange.map(_.toLong).bounds(scale)
      val (rand1, n) = rand0.nextLong(bounds)
      val (rand2, t0) = setReplicate(n.toInt, gen).run(rand1, scale)
      val t1 = t0.expand {
        case None => Seq.empty
        case Some(xs) => Shrink.list(min.toInt, xs).map(Some(_))
      }.map(_.map(_.toSet))
      (rand2, t1)
    }

  def mapOf[K, V](keyGen: Gen[K], valueGen: Gen[V], sizeRange: Range[Int]): Gen[Map[K, V]] =
    Gen { (rand0, scale) =>
      val bounds@(min, _) = sizeRange.map(_.toLong).bounds(scale)
      val (rand1, n) = rand0.nextLong(bounds)
      val (rand2, keys) = setReplicate(n.toInt, keyGen).run(rand1, scale)
      val (rand3, values) = replicate(n.toInt, valueGen).run(rand2, scale)
      val t = keys.productMap(values)((ks, vs) => for (k <- ks; v <- vs) yield k.zip(v)).expand {
        case None => Seq.empty
        case Some(xs) => Shrink.list(min.toInt, xs).map(Some(_))
      }.map(_.map(_.toMap))
      (rand3, t)
    }

  def tuple2[T1, T2](gen1: Gen[T1], gen2: Gen[T2]): Gen[(T1, T2)] =
    gen1.productMap(gen2)((_, _))

  def tuple3[T1, T2, T3](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3]): Gen[(T1, T2, T3)] =
    tuple2(gen1, gen2).productMap(gen3) { case ((x1, x2), x3) => (x1, x2, x3) }

  def tuple4[T1, T2, T3, T4](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4]): Gen[(T1, T2, T3, T4)] =
    tuple3(gen1, gen2, gen3).productMap(gen4) { case ((x1, x2, x3), x4) => (x1, x2, x3, x4) }
}
