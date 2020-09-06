package codes.quine.labo.hariko.data

/** Range is scalable bounds. */
final case class Range[T](base: T, run: Int => (T, T)) {

  /** Returns a bounds in scale. */
  def bounds(scale: Int)(implicit T: Ordering[T]): (T, T) = {
    require(0 <= scale && scale <= 100, "hariko.data.Random#bounds: invalid scale")
    val (x, y) = run(scale)
    val min = T.min(x, y)
    val max = T.max(x, y)
    assert(T.lteq(min, base) && T.lteq(base, max), "hariko.data.Random#bounds: bounds must contain base")
    (min, max)
  }

  /** Returns a new [[Range]] which value is converted by `f`. */
  def map[U](f: T => U): Range[U] =
    Range(f(base), scale => run(scale) match { case (x, y) => (f(x), f(y)) })
}

object Range {

  /** Returns a constant range. */
  def constant[T](min: T, max: T): Range[T] = constant(min, min, max)

  /** Returns a constant range. */
  def constant[T](base: T, min: T, max: T): Range[T] = Range(base, _ => (min, max))

  /** Returns a linear scaling range. */
  def linear(min: Byte, max: Byte): Range[Byte] = linear(min, min, max)

  /** Returns a linear scaling range. */
  def linear(min: Short, max: Short): Range[Short] = linear(min, min, max)

  /** Returns a linear scaling range. */
  def linear(min: Int, max: Int): Range[Int] = linear(min, min, max)

  /** Returns a linear scaling range. */
  def linear(min: Long, max: Long): Range[Long] = linear(min, min, max)

  /** Returns a linear scaling range. */
  def linear(base: Byte, min: Byte, max: Byte): Range[Byte] = linear(base.toLong, min.toLong, max.toLong).map(_.toByte)

  /** Returns a linear scaling range. */
  def linear(base: Short, min: Short, max: Short): Range[Short] =
    linear(base.toLong, min.toLong, max.toLong).map(_.toShort)

  /** Returns a linear scaling range. */
  def linear(base: Int, min: Int, max: Int): Range[Int] = linear(base.toLong, min.toLong, max.toLong).map(_.toInt)

  /** Returns a linear scaling range. */
  def linear(base: Long, min: Long, max: Long): Range[Long] = {
    require(min <= max, "hariko.data.Range.linear: invalid bounds")
    require(min <= base && base <= max, "hariko.data.Range.linear: invalid base")
    if (base - min < 0 || max - base < 0) linearBigInt(BigInt(base), BigInt(min), BigInt(max)).map(_.toLong)
    else
      Range(
        base,
        scale => (base - ((base - min) * (scale / 100.0)).toLong, base + ((max - base) * (scale / 100.0)).toLong)
      )
  }

  /** Returns a linear scaling range. */
  def linear(min: Float, max: Float): Range[Float] = linear(min, min, max)

  /** Returns a linear scaling range. */
  def linear(min: Double, max: Double): Range[Double] = linear(min, min, max)

  /** Returns a linear scaling range. */
  def linear(base: Float, min: Float, max: Float): Range[Float] =
    linear(base.toDouble, min.toDouble, max.toDouble).map(_.toFloat)

  /** Returns a linear scaling range. */
  def linear(base: Double, min: Double, max: Double): Range[Double] = {
    require(min <= max, "hariko.data.Range.linear: invalid bounds")
    require(min <= base && base <= max, "hariko.data.Range.linear: invalid base")
    Range(base, scale => (base - (base - min) * (scale / 100.0), base + (max - base) * (scale / 100.0)))
  }

  /**
    * Returns a linear scaling range of BigIng.
    *
    * It is private because this method is for preventing overflow.
    */
  private[this] def linearBigInt(base: BigInt, min: BigInt, max: BigInt): Range[BigInt] =
    Range(base, scale => (base - (base - min) * scale / 100, base + (max - base) * scale / 100))
}
