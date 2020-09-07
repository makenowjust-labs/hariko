package codes.quine.labo.hariko

import scala.concurrent.ExecutionContext.Implicits.global

import minitest.api.Asserts

trait HarikoChecker {
  import Asserts._

  val param: Param = Param(System.currentTimeMillis())

  def check(p: Property): Unit = {
    val result = p.run(param.toRandom, param)
    if (!result.isPass) fail(result.toString())
  }
}
