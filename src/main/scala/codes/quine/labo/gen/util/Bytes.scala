package codes.quine.labo.gen.util

object Bytes {
  def compose(x: (Byte, Byte)): Short =
    ((x._1 << 8) | x._2).toShort

  def compose(x: (Byte, Byte, Byte, Byte)): Int =
    (x._1 << 24) | (x._2 << 16) | (x._3 << 8) | x._4

  def compose(x: (Int, Int)): Long =
    (x._1.toLong << 32) | x._2

  def decompose(x: Short): (Byte, Byte) =
    ((x >>> 8).toByte, x.toByte)

  def decompose(x: Int): (Byte, Byte, Byte, Byte) =
    ((x >>> 24).toByte, (x >>> 16).toByte, (x >>> 8).toByte, x.toByte)

  def decompose(x: Long): (Int, Int) =
    ((x >>> 32).toInt, x.toInt)
}
