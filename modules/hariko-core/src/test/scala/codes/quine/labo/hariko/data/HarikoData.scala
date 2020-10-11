package codes.quine.labo.hariko
package data

/** HarikoData provides generators of `data` types.
  */
object HarikoData {

  /** A tree generator.
    */
  implicit def treeGen[T](implicit gen: Gen[T], sizeRange: Range[Int] = Range.constant(0, 3)): Gen[Tree[T]] =
    Gen.delay(Gen.map2(gen, Gen.list(treeGen(gen, sizeRange.map(_ / 2)), sizeRange).map(LazyList.from(_)))(Tree(_, _)))

  /** A range generator.
    */
  implicit def rangeGen[T](implicit gen: Gen[T]): Gen[Range[T]] =
    Gen.map2(gen, Gen.function1(Cogen.byte.imap(_.toInt)(_.toByte), Gen.tuple2(gen, gen)))(Range(_, _))
}
