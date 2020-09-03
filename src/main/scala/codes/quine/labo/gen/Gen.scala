package codes.quine.labo.gen

import data.Fun
import data.Range
import data.Tree
import data.PartialFun._
import data.UntupledFun._
import random.Random
import util.Shrink

/**
  * Gen is random value generator in scale with shrinking.
  *
  * Techenically, this type is isomorphic to `cats` type `ReaderT[(Param, Int), Nested[State[Random, *], Nested[Tree, Option, *], *], *]`.
  * Thus it is lawful as `Functor`, `Applicative` or `FunctorFilter`.
  */
trait Gen[T] {

  /**
    * Runs this generator with the given parameter.
    */
  def run(rand: Random, param: Param, scale: Int): (Random, Tree[Option[T]])

  /**
    * Runs this generator endlessly with the given parameter.
    *
    * Note that this result becomes infinite [[scala.collection.immutable.LazyList LazyList]].
    */
  def toLazyList(rand: Random, param: Param, scale: Int): LazyList[(Random, Tree[Option[T]])] =
    LazyList.iterate((rand, Tree.pure(Option.empty[T]))) { case (rand, _) => run(rand, param, scale) }.drop(1)

  /**
    * Generates values with the given parameter.
    *
    * It is for testing or demonstrating [[Gen]] behavior.
    */
  def samples(param: Param = Param(42)): LazyList[T] =
    toLazyList(param.toRandom, param, param.maxScale).map(_._2.value).collect { case Some(x) => x }

  def unsafeGet(rand: Random, param: Param, scale: Int): Option[(Random, Tree[T])] =
    toLazyList(rand, param, scale)
      .map { case (rand, t) => (rand, Tree.collapse(t)) }
      .take(param.maxDiscarded)
      .collect { case (rand, Some(t)) => (rand, t) }
      .headOption

  def mapTree[U](f: Tree[Option[T]] => Tree[Option[U]]): Gen[U] =
    Gen { (rand0, param, scale) =>
      val (rand1, t) = run(rand0, param, scale)
      (rand1, f(t))
    }

  /** Returns a new [[Gen]] which contains a value which is returned by `f` with generated value. */
  def map[U](f: T => U): Gen[U] =
    mapTree(_.map(_.map(f)))

  /** Returns a new [[Gen]] which contains a value which `true` is returned by `f` with generated value. */
  def filter(f: T => Boolean): Gen[T] =
    mapTree(_.map(_.filter(f)))

  /** Returns a new [[Gen]] which contains a value which `Some` is returned by `f` with generated value. */
  def mapFilter[U](f: T => Option[U]): Gen[U] = collect(f.unlift)

  /** Returns a new [[Gen]] which contains a value which is handled by `f` with generated value. */
  def collect[U](f: PartialFunction[T, U]): Gen[U] =
    mapTree(_.map(_.collect(f)))
}

/**
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
    * Creates a new [[Gen]] with given function.
    *
    * @group util
    */
  def apply[T](f: (Random, Param, Int) => (Random, Tree[Option[T]])): Gen[T] =
    new Gen[T] {
      def run(rand: Random, param: Param, scale: Int): (Random, Tree[Option[T]]) =
        f(rand, param, scale)
    }

  /**
    * Delays `gen` evaluation for recursive [[Gen]].
    *
    * @group util
    */
  def delay[T](gen: => Gen[T]): Gen[T] = {
    lazy val lazyGen = gen
    Gen((rand, param, scale) => lazyGen.run(rand, param, scale))
  }

  /**
    * Returns an empty [[Gen]].
    *
    * @group combinator
    */
  def empty[T]: Gen[T] = Gen((rand, _, _) => (rand, Tree.pure(None)))

  /**
    * Returns a [[Gen]] which contains `x` only.
    *
    * @group combinator
    */
  def pure[T](x: T): Gen[T] = Gen((rand, _, _) => (rand, Tree.pure(Some(x))))

  /**
    * Returns two [[Gen]]s product with mapping.
    *
    * It is the same as `Gen.tuple2(gen1, gen2).map(f.tupled)`
    *
    * @group combinator
    */
  def map2[T1, T2, U](gen1: Gen[T1], gen2: Gen[T2])(f: (T1, T2) => U): Gen[U] =
    Gen { (rand0, param, scale) =>
      val (rand1, t) = gen1.run(rand0, param, scale)
      val (rand2, u) = gen2.run(rand1, param, scale)
      (rand2, Tree.map2(t, u)((a, b) => for (x <- a; y <- b) yield f(x, y)))
    }

  /**
    * Returns three [[Gen]]s product with mapping.
    *
    * It is the same as `Gen.tuple3(gen1, gen2, gen3).map(f.tupled)`
    *
    * @group combinator
    */
  def map3[T1, T2, T3, U](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3])(f: (T1, T2, T3) => U): Gen[U] =
    map2(tuple2(gen1, gen2), gen3) { case ((x1, x2), x3) => f(x1, x2, x3) }

  /**
    * Returns three [[Gen]]s product with mapping.
    *
    * It is the same as `Gen.tuple4(gen1, gen2, gen3).map(f.tupled)`
    *
    * @group combinator
    */
  def map4[T1, T2, T3, T4, U](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4])(
      f: (T1, T2, T3, T4) => U
  ): Gen[U] =
    map2(tuple3(gen1, gen2, gen3), gen4) { case ((x1, x2, x3), x4) => f(x1, x2, x3, x4) }

  /**
    * Repeat `gen` with `n` times.
    *
    * @group combinator
    */
  def replicate[T](n: Int, gen: Gen[T]): Gen[List[T]] = {
    require(n >= 0, "gen.Gen.replicate: invalid replicate number")
    if (n == 0) Gen.pure(List.empty)
    else if (n == 1) gen.map(List(_))
    else if (n % 2 == 1) map2(gen, replicate(n - 1, gen))(_ +: _)
    else {
      val gen2 = replicate(n / 2, gen)
      map2(gen2, gen2)(_ ++ _)
    }
  }

  /**
    * Repeat `gen` with `n` times without generated value duplication.
    *
    * @group combinator
    */
  def setReplicate[T](n: Int, gen: Gen[T]): Gen[List[T]] = {
    require(n >= 0, "gen.Gen.setReplicate: invalid replicate number")
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
    * Creates a new [[Gen]] which choices a [[Gen]] following the given distributions.
    *
    * @group combinator
    */
  def frequency[T](dists: (Int, Gen[T])*): Gen[T] = {
    require(dists.nonEmpty, "gen.Gen.frequency: no distributions")
    require(dists.forall(_._1 >= 1), "gen.Gen.frequency: invalid distribution")
    val ps :+ total = dists.scanLeft(0)(_ + _._1)
    val pairs = ps.zip(dists.map(_._2))
    Gen { (rand0, param, scale) =>
      val (rand1, n) = rand0.nextLong((1, total))
      val (_, gen) = pairs.findLast(_._1 < n).get
      gen.run(rand1, param, scale)
    }
  }

  /**
    * Returns a unit [[Gen]].
    *
    * @group primitive
    */
  def unit: Gen[Unit] = pure(())

  /**
    * Returns a boolean [[Gen]].
    *
    * @group primitive
    */
  def boolean: Gen[Boolean] = long(Range.constant(0, 1)).map(_ == 1)

  /**
    * Returns a byte [[Gen]].
    *
    * @group primitive
    */
  def byte(range: Range[Byte]): Gen[Byte] = long(range.map(_.toLong)).map(_.toByte)

  /**
    * Returns a short [[Gen]].
    *
    * @group primitive
    */
  def short(range: Range[Short]): Gen[Short] = long(range.map(_.toLong)).map(_.toShort)

  /**
    * Returns an int [[Gen]].
    *
    * @group primitive
    */
  def int(range: Range[Int]): Gen[Int] = long(range.map(_.toLong)).map(_.toInt)

  /**
    * Returns a long [[Gen]].
    *
    * @group primitive
    */
  def long(range: Range[Long]): Gen[Long] =
    Gen { (rand0, _, scale) =>
      val (rand1, x) = rand0.nextLong(range.bounds(scale))
      val t = Tree.pure(x).expand(Shrink.long(range.base, _))
      (rand1, t.map(Some(_)))
    }

  /**
    * Returns a char [[Gen]].
    *
    * @group primitive
    */
  def char(range: Range[Char]): Gen[Char] =
    long(range.map(_.toLong)).map(_.toChar)

  /**
    * Returns a float [[Gen]].
    *
    * @group primitive
    */
  def float(range: Range[Float]): Gen[Float] =
    double(range.map(_.toFloat)).map(_.toFloat)

  /**
    * Returns a double [[Gen]].
    *
    * @group primitive
    */
  def double(range: Range[Double]): Gen[Double] =
    Gen { (rand0, _, scale) =>
      val (rand1, x) = rand0.nextDouble(range.bounds(scale))
      val t = Tree.pure(x).expand(Shrink.double(range.base, _))
      (rand1, t.map(Some(_)))
    }

  /**
    * Returns a string [[Gen]].
    *
    * @group collection
    */
  def string(charGen: Gen[Char], sizeRange: Range[Int]): Gen[String] =
    list(charGen, sizeRange).map(_.mkString)

  /**
    * Returns a list [[Gen]].
    *
    * @group collection
    */
  def list[T](gen: Gen[T], sizeRange: Range[Int]): Gen[List[T]] =
    Gen { (rand0, param, scale) =>
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
    * Returns a set [[Gen]].
    *
    * @group collection
    */
  def set[T](gen: Gen[T], sizeRange: Range[Int]): Gen[Set[T]] =
    Gen { (rand0, param, scale) =>
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
    * Returns a map [[Gen]].
    *
    * @group collection
    */
  def map[K, V](keyGen: Gen[K], valueGen: Gen[V], sizeRange: Range[Int]): Gen[Map[K, V]] =
    Gen { (rand0, param, scale) =>
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
    * Returns an option [[Gen]].
    *
    * @group collection
    */
  def option[T](gen: Gen[T]): Gen[Option[T]] =
    frequency(1 -> pure(None), 1 -> gen.map(Some(_)))

  /**
    * Returns an either [[Gen]].
    *
    * @group collection
    */
  def either[T, U](leftGen: Gen[T], rightGen: Gen[U]): Gen[Either[T, U]] =
    frequency(1 -> leftGen.map(Left(_)), 1 -> rightGen.map(Right(_)))

  /**
    * Returns a two elements tuple [[Gen]].
    *
    * @group collection
    */
  def tuple2[T1, T2](gen1: Gen[T1], gen2: Gen[T2]): Gen[(T1, T2)] =
    map2(gen1, gen2)((_, _))

  /**
    * Returns a three elements tuple [[Gen]].
    *
    * @group collection
    */
  def tuple3[T1, T2, T3](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3]): Gen[(T1, T2, T3)] =
    map2(tuple2(gen1, gen2), gen3) { case ((x1, x2), x3) => (x1, x2, x3) }

  /**
    * Returns a four elements tuple [[Gen]].
    *
    * @group collection
    */
  def tuple4[T1, T2, T3, T4](gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4]): Gen[(T1, T2, T3, T4)] =
    map2(tuple3(gen1, gen2, gen3), gen4) { case ((x1, x2, x3), x4) => (x1, x2, x3, x4) }

  /**
    * Returns a [[PartialFun]] [[Gen]].
    *
    * @group function
    */
  def partialFun[T, R](cogen: Cogen[T], gen: Gen[R]): Gen[T :=> Option[R]] =
    Gen((rand0, param, scale) => cogen.build(gen).run(rand0, param, scale))

  /**
    * Returns a [[Fun]] [[Gen]].
    *
    * @group function
    */
  def fun[T, R](cogen: Cogen[T], gen: Gen[R]): Gen[Fun[T, R]] =
    map2(partialFun(cogen, gen), gen)(Fun(_, _, false))
      // Mark children as shurnk for better `toString`.
      .mapTree { case Tree(x, xs) => Tree(x, xs.map(_.map(_.map(_.copy(isShrunk = true))))) }

  /**
    * Returns a function [[Gen]].
    *
    * @group function
    */
  def function1[T1, R](cogen1: Cogen[T1], gen: Gen[R]): Gen[T1 => R] =
    fun(cogen1, gen).map(_.asInstanceOf[T1 => R])

  /**
    * Returns a two inputs function [[Gen]].
    *
    * @group function
    */
  def function2[T1, T2, R](cogen1: Cogen[T1], cogen2: Cogen[T2], gen: Gen[R]): Gen[(T1, T2) => R] =
    fun(Cogen.tuple2(cogen1, cogen2), gen).map(new UntupledFun2(_))

  /**
    * Returns a three inputs function [[Gen]].
    *
    * @group function
    */
  def function3[T1, T2, T3, R](
      cogen1: Cogen[T1],
      cogen2: Cogen[T2],
      cogen3: Cogen[T3],
      gen: Gen[R]
  ): Gen[(T1, T2, T3) => R] =
    fun(Cogen.tuple3(cogen1, cogen2, cogen3), gen).map(new UntupledFun3(_))

  /**
    * Returns a four inputs function [[Gen]].
    *
    * @group function
    */
  def function4[T1, T2, T3, T4, R](
      cogen1: Cogen[T1],
      cogen2: Cogen[T2],
      cogen3: Cogen[T3],
      cogen4: Cogen[T4],
      gen: Gen[R]
  ): Gen[(T1, T2, T3, T4) => R] =
    fun(Cogen.tuple4(cogen1, cogen2, cogen3, cogen4), gen).map(new UntupledFun4(_))
}
