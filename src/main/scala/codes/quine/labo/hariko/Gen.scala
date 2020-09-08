package codes.quine.labo.hariko

import scala.annotation.implicitNotFound

import data.Fun
import data.Range
import data.Tree
import data.PartialFun._
import data.UntupledFun._
import random.Random
import util.Shrink

/**
  * Gen is a random value generator integrated with shrinking.
  *
  * Techenically, this type is isomorphic to `ReaderT[(Param, Int), Nested[State[Random, *], Nested[Tree, Option, *], *], *]`.
  * Thus it is lawful as `Functor`, `Applicative` or `FunctorFilter`.
  */
@implicitNotFound("no default generator of ${T}")
trait Gen[T] {

  /**
    * Generate a random value along with parameters.
    *
    * It take three parameters:
    *
    *   - The first is PRNG for this generation.
    *   - The second is parameter of this generation.
    *   - The third is scale of this generation. It bounds `[0, 100]`.
    *
    * It returns a pair:
    *
    *   - The first is PRNG after this generation.
    *   - The second is shrinking tree. The root becomes generated value.
    */
  def run(rand: Random, param: Param, scale: Int): (Random, Tree[Option[T]])

  /**
    * Runs this generator endlessly along with parameters.
    *
    * NOTE: this result becomes an infinite `LazyList`.
    */
  def toLazyList(rand: Random, param: Param, scale: Int): LazyList[(Random, Tree[Option[T]])] =
    LazyList.iterate((rand, Tree.pure(Option.empty[T]))) { case (rand, _) => run(rand, param, scale) }.drop(1)

  /**
    * Generates values along with the parameter.
    *
    * It is for testing or demonstrating [[Gen]] behavior.
    */
  def samples(param: Param = Param(42)): LazyList[T] =
    toLazyList(param.toRandom, param, param.maxScale).map(_._2.value).collect { case Some(x) => x }

  /**
    * Selects the first valid generated value from this generator.
    *
    * It tries to generate values at most `param.maxDiscarded` times.
    * If there is no valid value in these values, it returns `None` instead.
    *
    * NOTE: this result tree is collapsed, so it makes shirinking poor when
    * this method is used in generator implementation.
    */
  def unsafeHead(rand: Random, param: Param, scale: Int): Option[(Random, Tree[T])] =
    toLazyList(rand, param, scale)
      .map { case (rand, t) => (rand, Tree.collapse(t)) }
      .take(param.maxDiscarded)
      .collect { case (rand, Some(t)) => (rand, t) }
      .headOption

  /**
    * Like `gen.map(f)`, but it maps over trees.
    */
  def mapTree[U](f: Tree[Option[T]] => Tree[Option[U]]): Gen[U] =
    Gen.from { (rand0, param, scale) =>
      val (rand1, t) = run(rand0, param, scale)
      (rand1, f(t))
    }

  /**
    * Builds a new generator applied the function to each generated values.
    *
    * Example:
    *
    * {{{
    * scala> Gen.int(Range.linear(0, 10)).map(_ + 1).samples().take(10).toList
    * res0: List[Int] = List(4, 9, 9, 3, 5, 8, 8, 11, 5, 1)
    * }}}
    */
  def map[U](f: T => U): Gen[U] =
    mapTree(_.map(_.map(f)))

  /**
    * Selects generated values of this generator which satisfy a predicate.
    *
    * Example:
    *
    * {{{
    * scala> Gen.int(Range.linear(0, 10)).filter(_ % 2 == 0).samples().take(10).toList
    * res0: List[Int] = List(8, 8, 2, 4, 10, 4, 0, 4, 10, 6)
    * }}}
    */
  def filter(p: T => Boolean): Gen[T] =
    mapTree(_.map(_.filter(p)))

  /**
    * Like `gen.filter(p)`, but it is combined with `map`.
    *
    * Filtering is handled via `Option` instead of `Boolean` such that
    * output type `U` can be different than input type `T`.
    */
  def mapFilter[U](p: T => Option[U]): Gen[U] = collect(p.unlift)

  /**
    * Like `gen.mapFilter(p)`, but it accepts `PartialFunction` instead.
    */
  def collect[U](pf: PartialFunction[T, U]): Gen[U] =
    mapTree(_.map(_.collect(pf)))
}

/**
  * Utilities for generator.
  *
  * @groupname util Utility Functions
  * @groupprio util 0
  *
  * @groupname combinator Combinators
  * @groupprio combinator 100
  *
  * @groupname primitive Primitive Generators
  * @groupprio primitive 200
  *
  * @groupname collection Collection Generators
  * @groupprio collection 210
  *
  * @groupname function Function Generators
  * @groupprio function 220
  */
object Gen {

  /**
    * Summons generator instance of type `T`.
    */
  def apply[T](implicit gen: Gen[T]): Gen[T] = gen

  /**
    * Creates a new generator from the function.
    *
    * See [[Gen#run]] for detailed explanation of function parameters and result.
    *
    * @group util
    */
  def from[T](f: (Random, Param, Int) => (Random, Tree[Option[T]])): Gen[T] =
    new Gen[T] {
      def run(rand: Random, param: Param, scale: Int): (Random, Tree[Option[T]]) =
        f(rand, param, scale)
    }

  /**
    * Delays `gen` evaluation for recursive generator.
    *
    * @group util
    */
  def delay[T](gen: => Gen[T]): Gen[T] = {
    lazy val lazyGen = gen
    Gen.from((rand, param, scale) => lazyGen.run(rand, param, scale))
  }

  /**
    * Creates an empty generator.
    *
    * @group combinator
    */
  def empty[T]: Gen[T] = Gen.from((rand, _, _) => (rand, Tree.pure(None)))

  /**
    * Creates a constant generator that generates `x` always.
    *
    * @group combinator
    */
  def pure[T](x: T): Gen[T] = Gen.from((rand, _, _) => (rand, Tree.pure(Some(x))))

  /**
    * Builds two generators product with the mapping.
    *
    * It is equivalent to `Gen.tuple2(gen1, gen2).map(f.tupled)`
    *
    * Example:
    *
    * {{{
    * scala> Gen.map2(Gen.int(Range.linear(0, 10)), Gen.boolean) { (k, v) =>
    *      |   Map(k -> v)
    *      | }.samples().take(3).toList
    * res0: List[Map[Int, Boolean]] = List(Map(3 -> true), Map(8 -> true), Map(4 -> true))
    * }}}
    *
    * NOTE: this and `tuple2` are associative. In short, it holds
    * `Gen.map2(Gen.tuple2(gen1, gen2), gen3) { case ((x, y), z) => (x, y, z) } == Gen.map2(gen1, Gen.tuple2(gen2, gen3)) { case (x, (y, z)) => (x, y, z) }`.
    *
    * @group combinator
    */
  def map2[T1, T2, U](gen1: Gen[T1], gen2: Gen[T2])(f: (T1, T2) => U): Gen[U] =
    Gen.from { (rand0, param, scale) =>
      val (rand1, t) = gen1.run(rand0, param, scale)
      val (rand2, u) = gen2.run(rand1, param, scale)
      (rand2, Tree.map2(t, u)((a, b) => for (x <- a; y <- b) yield f(x, y)))
    }

  /**
    * Builds three generators product with the mapping.
    *
    * @group combinator
    */
  def map3[T1, T2, T3, U](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3])(f: (T1, T2, T3) => U): Gen[U] =
    map2(tuple2(gen1, gen2), gen3) { case ((x1, x2), x3) => f(x1, x2, x3) }

  /**
    * Builds four generators product with the mapping.
    *
    * @group combinator
    */
  def map4[T1, T2, T3, T4, U](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4])(
      f: (T1, T2, T3, T4) => U
  ): Gen[U] =
    map2(tuple3(gen1, gen2, gen3), gen4) { case ((x1, x2, x3), x4) => f(x1, x2, x3, x4) }

  /**
    * Repeats the generator with `n` times.
    *
    * @group combinator
    */
  def replicate[T](n: Int, gen: Gen[T]): Gen[List[T]] = {
    require(n >= 0, "hariko.Gen.replicate: invalid replicate number")
    if (n == 0) Gen.pure(List.empty)
    else if (n == 1) gen.map(List(_))
    else if (n % 2 == 1) map2(gen, replicate(n - 1, gen))(_ +: _)
    else {
      val gen2 = replicate(n / 2, gen)
      map2(gen2, gen2)(_ ++ _)
    }
  }

  /**
    * Repeats the generator with `n` times without generated value duplication.
    *
    * @group combinator
    */
  def setReplicate[T](n: Int, gen: Gen[T]): Gen[List[T]] = {
    require(n >= 0, "hariko.Gen.setReplicate: invalid replicate number")
    if (n == 0) Gen.pure(List.empty)
    else if (n == 1) gen.map(List(_))
    else if (n % 2 == 1)
      tuple2(gen, setReplicate(n - 1, gen)).collect { case (x, xs) if !xs.contains(x) => x :: xs }
    else {
      val gen2 = setReplicate(n / 2, gen)
      tuple2(gen2, gen2).collect { case (xs1, xs2) if xs1.forall(!xs2.contains(_)) => xs1 ++ xs2 }
    }
  }

  /**
    * Builds a new generator which choices a value according to distribution.
    *
    * For example, `Gen.frequency(1 -> genA, 2 -> genB)` choices
    * `genA` and `genB` values according to `1:2` ratio.
    *
    * Example:
    *
    * {{{
    * scala> Gen.frequency(3 -> Gen.pure("fizz"), 5 -> Gen.pure("buzz")).samples().take(10).toList
    * res0: List[String] = List(fizz, buzz, buzz, buzz, fizz, fizz, fizz, buzz, buzz, buzz)
    * }}}
    *
    * @group combinator
    */
  def frequency[T](dist: (Int, Gen[T])*): Gen[T] = {
    require(dist.nonEmpty, "hariko.Gen.frequency: no distribution")
    require(dist.forall(_._1 >= 1), "hariko.Gen.frequency: invalid distribution")
    val ps :+ total = dist.scanLeft(0)(_ + _._1)
    val pairs = ps.zip(dist.map(_._2))
    Gen.from { (rand0, param, scale) =>
      val (rand1, n) = rand0.nextLong((1, total))
      val (_, gen) = pairs.findLast(_._1 < n).get
      gen.run(rand1, param, scale)
    }
  }

  /**
    * A unit generator.
    *
    * @group primitive
    */
  implicit def unit: Gen[Unit] = pure(())

  /**
    * A boolean generator.
    *
    * Example:
    *
    * {{{
    * scala> Gen.boolean.samples().take(10).toList
    * res0: List[Boolean] = List(false, true, true, true, false, true, false, true, true, true)
    * }}}
    *
    * @group primitive
    */
  implicit def boolean: Gen[Boolean] = long(Range.constant(0, 1)).map(_ == 1)

  /**
    * A byte generator in range.
    *
    * @group primitive
    */
  def byte(range: Range[Byte]): Gen[Byte] = long(range.map(_.toLong)).map(_.toByte)

  /**
    * Alias of `Gen.byte(Range.linear(0, Byte.MinValue, Byte.MaxValue))`.
    *
    * @group primitive
    */
  implicit def byte: Gen[Byte] = byte(Range.linear(0, Byte.MinValue, Byte.MaxValue))

  /**
    * A short generator in range.
    *
    * @group primitive
    */
  def short(range: Range[Short]): Gen[Short] = long(range.map(_.toLong)).map(_.toShort)

  /**
    * Alias of `Gen.short(Range.linear(0, Short.MinValue, Short.MaxValue))`.
    *
    * @group primitive
    */
  implicit def short: Gen[Short] = short(Range.linear(0, Short.MinValue, Short.MaxValue))

  /**
    * An int generator in range.
    *
    * Example:
    *
    * {{{
    * scala> Gen.int(Range.linear(0, 10)).samples().take(10).toList
    * res0: List[Int] = List(3, 8, 8, 2, 4, 7, 7, 10, 4, 0)
    * }}}
    *
    * @group primitive
    */
  def int(range: Range[Int]): Gen[Int] = long(range.map(_.toLong)).map(_.toInt)

  /**
    * Alias of `Gen.int(Range.linear(0, Int.MinValue, Int.MaxValue))`.
    *
    * @group primitive
    */
  implicit def int: Gen[Int] = Gen.int(Range.linear(0, Int.MinValue, Int.MaxValue))

  /**
    * A long generator in range.
    *
    * @group primitive
    */
  def long(range: Range[Long]): Gen[Long] =
    Gen.from { (rand0, _, scale) =>
      val (rand1, x) = rand0.nextLong(range.bounds(scale))
      val t = Tree.pure(x).expand(Shrink.long(range.base, _))
      (rand1, t.map(Some(_)))
    }

  /**
    * Alias of `Gen.long(Range.linear(0, Long.MinValue, Long.MaxValue))`.
    *
    * @group primitive
    */
  implicit def long: Gen[Long] = Gen.long(Range.linear(0, Long.MinValue, Long.MaxValue))

  /**
    * A char generator in range.
    *
    * @group primitive
    */
  def char(range: Range[Char]): Gen[Char] =
    long(range.map(_.toLong)).map(_.toChar)

  /**
    * Alias of `Gen.char(Range.constant(' ', '~'))`.
    *
    * @group primitive
    */
  implicit def char: Gen[Char] = char(Range.constant(' ', '~'))

  /**
    * A float generator in range.
    *
    * @group primitive
    */
  def float(range: Range[Float]): Gen[Float] =
    double(range.map(_.toFloat)).map(_.toFloat)

  /**
    * Alias of `Gen.float(Range.linear(0, Float.MinValue, Float.MaxValue))`.
    *
    * @group primitive
    */
  implicit def float: Gen[Float] = float(Range.linear(0, Float.MinValue, Float.MaxValue))

  /**
    * A double generator in range.
    *
    * @group primitive
    */
  def double(range: Range[Double]): Gen[Double] =
    Gen.from { (rand0, _, scale) =>
      val (rand1, x) = rand0.nextDouble(range.bounds(scale))
      val t = Tree.pure(x).expand(Shrink.double(range.base, _))
      (rand1, t.map(Some(_)))
    }

  /**
    * Alias of `Gen.double(Range.linear(0, Double.MinValue, Double.MaxValue))`.
    *
    * @group primitive
    */
  implicit def double: Gen[Double] = Gen.double(Range.linear(0, Double.MinValue, Double.MaxValue))

  /**
    * A string generator.
    *
    * A generated string consists `charGen` values, and its size is in `sizeRange`.
    *
    * Example:
    *
    * {{{
    * scala> Gen.string(Gen.char(Range.constant('a', 'z')), Range.constant(3, 5)).samples().take(10).toList
    * res0: List[String] = List(lbd, nen, vev, ariiz, zapx, npt, omnjt, tsjnr, irm, twb)
    * }}}
    *
    * @group collection
    */
  implicit def string(implicit charGen: Gen[Char], sizeRange: Range[Int] = Range.constant(0, 30)): Gen[String] =
    list(charGen, sizeRange).map(_.mkString)

  /**
    * A list generator.
    *
    * A generated list consists `gen` values, and its size is in `sizeRange`.
    *
    * Example:
    *
    * {{{
    * scala> Gen.list(Gen.int(Range.linear(0, 10)), Range.constant(3, 5)).samples().take(4).toList
    * res0: List[List[Int]] = List(List(8, 8, 2), List(7, 7, 10), List(0, 4, 10), List(7, 6, 1, 1, 0))
    * }}}
    *
    * @group collection
    */
  implicit def list[T](implicit gen: Gen[T], sizeRange: Range[Int] = Range.constant(0, 30)): Gen[List[T]] =
    Gen.from { (rand0, param, scale) =>
      val bounds @ (min, _) = sizeRange.map(_.toLong).bounds(scale)
      val (rand1, n) = rand0.nextLong(bounds)
      val (rand2, t0) = replicate(n.toInt, gen).run(rand1, param, scale)
      val t1 = t0.expand {
        case None     => Seq.empty
        case Some(xs) => Shrink.list(min.toInt, xs).map(Some(_))
      }
      (rand2, t1)
    }

  /**
    * A set generator.
    *
    * A generated set consists `gen` values, and its size is in `sizeRange`.
    *
    * Example:
    *
    * {{{
    * scala> Gen.set(Gen.int(Range.linear(0, 10)), Range.constant(3, 5)).samples().take(4).toList
    * res0: List[Set[Int]] = List(Set(0, 4, 10), Set(4, 10, 3, 6), Set(6, 2, 5), Set(10, 7, 4))
    * }}}
    *
    * @group collection
    */
  implicit def set[T](implicit gen: Gen[T], sizeRange: Range[Int] = Range.constant(0, 30)): Gen[Set[T]] =
    Gen.from { (rand0, param, scale) =>
      val bounds @ (min, _) = sizeRange.map(_.toLong).bounds(scale)
      val (rand1, n) = rand0.nextLong(bounds)
      val (rand2, t0) = setReplicate(n.toInt, gen).run(rand1, param, scale)
      val t1 = t0
        .expand {
          case None     => Seq.empty
          case Some(xs) => Shrink.list(min.toInt, xs).map(Some(_))
        }
        .map(_.map(_.toSet))
      (rand2, t1)
    }

  /**
    * A map generator.
    *
    * Keys are generated from `keyGen`, and values are generated from `valueGen`.
    * A generated map's size is in `sizeRange`.
    *
    * Example:
    *
    * {{{
    * scala> Gen.map(Gen.int(Range.linear(0, 10)), Gen.boolean, Range.linear(2, 3)).samples().take(2).toList
    * res0: List[Map[Int, Boolean]] = List(Map(7 -> true, 10 -> false, 4 -> true), Map(7 -> false, 6 -> true, 1 -> false))
    * }}}
    *
    * @group collection
    */
  implicit def map[K, V](implicit
      keyGen: Gen[K],
      valueGen: Gen[V],
      sizeRange: Range[Int] = Range.constant(0, 30)
  ): Gen[Map[K, V]] =
    Gen.from { (rand0, param, scale) =>
      val bounds @ (min, _) = sizeRange.map(_.toLong).bounds(scale)
      val (rand1, n) = rand0.nextLong(bounds)
      val (rand2, keys) = setReplicate(n.toInt, keyGen).run(rand1, param, scale)
      val (rand3, values) = replicate(n.toInt, valueGen).run(rand2, param, scale)
      val t = Tree
        .map2(keys, values)((ks, vs) => for (k <- ks; v <- vs) yield k.zip(v))
        .expand {
          case None     => Seq.empty
          case Some(xs) => Shrink.list(min.toInt, xs).map(Some(_))
        }
        .map(_.map(_.toMap))
      (rand3, t)
    }

  /**
    * An option generator.
    *
    * It generates `None` and `Some(x)` according to `1:9` ratio.
    * `gen` is generator for `Some(x)` values.
    *
    * Example:
    *
    * {{{
    * scala> Gen.option(Gen.boolean).samples().take(5).toList
    * res0: List[Option[Boolean]] = List(Some(true), Some(true), None, Some(false), Some(true))
    * }}}
    *
    * @group collection
    */
  implicit def option[T](implicit gen: Gen[T]): Gen[Option[T]] =
    frequency(1 -> pure(None), 9 -> gen.map(Some(_)))

  /**
    * An either generator.
    *
    * It generates `Left(x)` and `Right(x)` according to `1:1` ratio.
    * `leftGen` is generator for `Left(x)` values, and `rightGen` is generator for `Right(x)` values.
    *
    * Example:
    *
    * {{{
    * scala> Gen.either(Gen.boolean, Gen.boolean).samples().take(4).toList
    * res0: List[Either[Boolean, Boolean]] = List(Left(true), Right(true), Left(true), Left(true))
    * }}}
    *
    * @group collection
    */
  implicit def either[T, U](implicit leftGen: Gen[T], rightGen: Gen[U]): Gen[Either[T, U]] =
    frequency(1 -> leftGen.map(Left(_)), 1 -> rightGen.map(Right(_)))

  /**
    * A two elements tuple generator.
    *
    * Example:
    *
    * {{{
    * scala> Gen.tuple2(Gen.int(Range.linear(0, 10)), Gen.boolean).samples().take(5).toList
    * res9: List[(Int, Boolean)] = List((3,true), (8,true), (4,true), (7,true), (4,true))
    * }}}
    *
    * @group collection
    */
  implicit def tuple2[T1, T2](implicit gen1: Gen[T1], gen2: Gen[T2]): Gen[(T1, T2)] =
    map2(gen1, gen2)((_, _))

  /**
    * A three elements tuple generator.
    *
    * @group collection
    */
  implicit def tuple3[T1, T2, T3](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3]): Gen[(T1, T2, T3)] =
    map2(tuple2(gen1, gen2), gen3) { case ((x1, x2), x3) => (x1, x2, x3) }

  /**
    * A four elements tuple generator.
    *
    * @group collection
    */
  implicit def tuple4[T1, T2, T3, T4](implicit
      gen1: Gen[T1],
      gen2: Gen[T2],
      gen3: Gen[T3],
      gen4: Gen[T4]
  ): Gen[(T1, T2, T3, T4)] =
    map2(tuple3(gen1, gen2, gen3), gen4) { case ((x1, x2, x3), x4) => (x1, x2, x3, x4) }

  /**
    * A [[data.PartialFun PartialFun]] generator.
    *
    * @group function
    */
  private def partialFun[T, R](cogen: Cogen[T], gen: Gen[R]): Gen[T :=> Option[R]] =
    Gen((rand0, param, scale) => cogen.build(gen).run(rand0, param, scale))

  /**
    * A [[data.Fun Fun]] generator.
    *
    * @group function
    */
  private def fun[T, R](cogen: Cogen[T], gen: Gen[R]): Gen[Fun[T, R]] =
    map2(partialFun(cogen, gen), gen)(Fun(_, _))

  /**
    * A function generator.
    *
    * Example:
    *
    * {{{
    * scala> Gen.function1(Cogen.boolean, Gen.boolean).samples().take(2).toList
    * res0: List[Function1[Boolean, Boolean]] = List({case false | true => true; case _ => true}, {case true => false; case false => true; case _ => true})
    * }}}
    *
    * @group function
    */
  implicit def function1[T1, R](implicit cogen1: Cogen[T1], gen: Gen[R]): Gen[T1 => R] =
    fun(cogen1, gen).map(_.asInstanceOf[T1 => R])

  /**
    * A two inputs function generator.
    *
    * @group function
    */
  implicit def function2[T1, T2, R](implicit cogen1: Cogen[T1], cogen2: Cogen[T2], gen: Gen[R]): Gen[(T1, T2) => R] =
    fun(Cogen.tuple2(cogen1, cogen2), gen).map(new UntupledFun2(_))

  /**
    * A three inputs function generator.
    *
    * @group function
    */
  implicit def function3[T1, T2, T3, R](implicit
      cogen1: Cogen[T1],
      cogen2: Cogen[T2],
      cogen3: Cogen[T3],
      gen: Gen[R]
  ): Gen[(T1, T2, T3) => R] =
    fun(Cogen.tuple3(cogen1, cogen2, cogen3), gen).map(new UntupledFun3(_))

  /**
    * A four inputs function generator.
    *
    * @group function
    */
  implicit def function4[T1, T2, T3, T4, R](implicit
      cogen1: Cogen[T1],
      cogen2: Cogen[T2],
      cogen3: Cogen[T3],
      cogen4: Cogen[T4],
      gen: Gen[R]
  ): Gen[(T1, T2, T3, T4) => R] =
    fun(Cogen.tuple4(cogen1, cogen2, cogen3, cogen4), gen).map(new UntupledFun4(_))
}
