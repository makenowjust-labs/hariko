package codes.quine.labo.gen

import java.util.concurrent.TimeoutException

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.blocking
import scala.util.control.NonFatal

import data.Tree
import random.Random

final case class Property(run: (Random, Param, Int, Long) => (Random, Property.Result)) {
  def run(param: Param)(implicit ec: ExecutionContext): Property.Result = {
    val scaleStep = Math.min(1, (param.maxScale - param.minScale) / param.minSuccessful)
    val start = System.currentTimeMillis()

    @tailrec def loop(rand: Random, scale: Int, successful: Int = 0, discard: Int = 0): Property.Result =
      if (System.currentTimeMillis() - start >= param.timeout.toMillis) Property.Timeout
      else if (successful >= param.minSuccessful) Property.Pass
      else if (discard >= param.maxDiscarded) Property.Discarded
      else
        run(rand, param, scale, start) match {
          case (rand, Property.Pass) =>
            loop(rand, Math.min(param.maxScale, scale + scaleStep), successful + 1, 0)
          case (rand, Property.Discarded) =>
            loop(rand, Math.min(param.maxScale, scale + scaleStep), successful, discard + 1)
          case (_, result) => result
        }

    val future = Future(blocking(loop(param.toRandom, param.minScale)))
    try Await.result(future, param.timeout)
    catch {
      case _: TimeoutException => Property.Timeout
    }
  }
}

object Property {
  sealed abstract class Result(val isSuccessful: Boolean) extends Serializable with Product
  final case object Pass extends Result(true)
  final case class CounterExample(value: Any) extends Result(false)
  final case object Discarded extends Result(false)
  final case class Error(ex: Throwable) extends Result(false)
  final case object Timeout extends Result(false)

  def forAll[T](gen: Gen[T])(f: T => Boolean): Property =
    Property { (rand0, param, scale, start) =>
      if (System.currentTimeMillis() - start >= param.timeout.toMillis) (rand0, Timeout)
      else {
        val (rand1, t) = gen.run(rand0, param, scale)
        if (System.currentTimeMillis() - start >= param.timeout.toMillis) (rand1, Timeout)
        else
          try t.value match {
            case Some(x) => if (f(x)) (rand1, Pass) else (rand1, shrinkCounterExample(t, param, start, f))
            case None    => (rand1, Discarded)
          } catch {
            case NonFatal(ex) => (rand1, Error(ex))
          }
      }
    }

  private def shrinkCounterExample[T](tree: Tree[Option[T]], param: Param, start: Long, f: T => Boolean): Result = {
    @tailrec def loop(
        children: LazyList[Tree[Option[T]]],
        result: Result,
        shrink: Int = 0,
        discard: Int = 0,
        stack: List[(Int, LazyList[Tree[Option[T]]])] = List.empty
    ): Result =
      if (System.currentTimeMillis() - start >= param.timeout.toMillis) Timeout
      else if (shrink >= param.maxShrink) result
      else if (discard >= param.maxDiscarded) stack match {
        case (d, ts) :: s => loop(ts, result, shrink, d, s)
        case Nil          => result
      }
      else
        children.headOption match {
          case Some(Tree(None, ts))             => loop(ts, result, shrink + 1, discard + 1, (discard, children.tail) :: stack)
          case Some(Tree(Some(x), ts)) if !f(x) => loop(ts, CounterExample(x), shrink + 1, discard, List.empty)
          case Some(_)                          => loop(children.tail, result, shrink + 1, discard, stack)
          case None                             => result
        }
    loop(tree.children, CounterExample(tree.value.get))
  }
}
