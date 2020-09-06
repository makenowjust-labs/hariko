package codes.quine.labo.hariko.util

/**
  * Utilities for bytes sequence from/to integer types convertion.
  *
  * Generally, it holds `Bytes.decompose(Bytes.compose(x)) == x` and `Bytes.compose(Bytes.decompose(x)) = x`.
  */
object Bytes {

  /**
    * Calculates a short value from two bytes.
    */
  def compose(x: (Byte, Byte)): Short =
    ((x._1 << 8) | x._2).toShort

  /**
    * Calculates an int value from four bytes.
    */
  def compose(x: (Byte, Byte, Byte, Byte)): Int =
    (x._1 << 24) | (x._2 << 16) | (x._3 << 8) | x._4

  /**
    * Calculates a long value from two ints.
    */
  def compose(x: (Int, Int)): Long =
    (x._1.toLong << 32) | x._2

  /**
    * Calculates two bytes from a short value.
    */
  def decompose(x: Short): (Byte, Byte) =
    ((x >>> 8).toByte, x.toByte)

  /**
    * Calculates four bytes from an int value.
    */
  def decompose(x: Int): (Byte, Byte, Byte, Byte) =
    ((x >>> 24).toByte, (x >>> 16).toByte, (x >>> 8).toByte, x.toByte)

  /**
    * Calculates two ints from a long value.
    */
  def decompose(x: Long): (Int, Int) =
    ((x >>> 32).toInt, x.toInt)
}
