package codes.quine.labo.hariko
package minitest

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

import _root_.minitest.api.Asserts

/**
  * A mix-in for writing Hariko property testing in minitest.
  */
trait HarikoChecker {
  import Asserts._

  /**
    * A default parameter of property testing.
    */
  val param: Param = Param()

  /**
    * A default parameter of coverage testing.
    *
    * For reprucible testing, its seed should be constant.
    */
  val paramForCoverage: Param = Param(42)

  /**
    * Runs the property.
    */
  def check(p: Property): Unit = {
    val result = p.run(param.toRandom, param)
    if (!result.isPass) fail(result.toString)
  }

  /**
    * Checks coverage of generated values for the generator.
    */
  def checkCoverageWith[T](gen: Gen[T], param: Param = paramForCoverage)(
      covers: ((Int, String), (T => Boolean))*
  ): Unit = {
    val n = param.minSuccessful // number to generate for testing
    val counts = mutable.IndexedSeq.fill(covers.size)(0)

    // Runs the generator.
    for (x <- gen.samples(param).take(n)) {
      for ((f, k) <- covers.map(_._2).zipWithIndex) {
        if (f(x)) counts(k) += 1
      }
    }

    // Checks coverage.
    for (((need, name), k) <- covers.map(_._1).zipWithIndex) {
      val r = counts(k).toDouble / n * 100
      if (r < need) fail(s"coverage not enough: $name [${counts(k)}/$n = $r% < $need%]")
      // `need == 0` is special, it means invalid generated value.
      if (need == 0 && r > 0) fail(s"coverage invalid: $name")
    }
  }

  /**
    * Checks coverage of generated values for the default generator.
    */
  def checkCoverage[T: Gen](covers: ((Int, String), (T => Boolean))*): Unit =
    checkCoverageWith(Gen[T])(covers: _*)
}
