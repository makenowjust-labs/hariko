package codes.quine.labo.hariko

import scala.concurrent.ExecutionContext.Implicits.global

import minitest.api.Asserts

/**
  * A mix-in for writing Hariko property testing in minitest.
  */
trait HarikoChecker {
  import Asserts._

  val param: Param = Param(System.currentTimeMillis())

  def check(p: Property): Unit = {
    val result = p.run(param.toRandom, param)
    if (!result.isPass) fail(result.toString())
  }
}
