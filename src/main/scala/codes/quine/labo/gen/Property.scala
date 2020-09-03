package codes.quine.labo.gen

import java.util.concurrent.TimeoutException

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.blocking
import scala.util.DynamicVariable
import scala.util.control.NonFatal

import data.Tree
import random.Random

final case class Property(run: (Random, Param, ExecutionContext) => (Random, Property.Result)) {

  def run(param: Param)(implicit ec: ExecutionContext): (Random, Property.Result) = run(param.toRandom, param)

  def run(rand: Random, param: Param)(implicit ec: ExecutionContext): (Random, Property.Result) = run(rand, param, ec)

  def withParam(f: Param => Param): Property =
    Property((rand, param, ec) => run(rand, f(param), ec))
}

object Property {
  sealed abstract class Result(val isSuccessful: Boolean) extends Serializable with Product
  final case object Pass extends Result(true)
  final case class CounterExample(value: Any) extends Result(false)
  final case object Discarded extends Result(false)
  final case class Error(ex: Throwable) extends Result(false)
  final case object Timeout extends Result(false)

  def forAll[T](gen: Gen[T])(f: T => Boolean): Property =
    Property { (rand, param, ec) =>
      def run(rand0: Random, scale: Int): (Random, Property.Result) =
        if (checkTimeout(param)) (rand0, Timeout)
        else {
          val (rand1, t) = gen.run(rand0, param, scale)
          if (checkTimeout(param)) (rand1, Timeout)
          else
            try t.value match {
              case Some(x) => if (f(x)) (rand1, Pass) else (rand1, shrinkCounterExample(t, param, f))
              case None    => (rand1, Discarded)
            } catch {
              case NonFatal(ex) => (rand1, Error(ex))
            }
        }

      val scaleStep = Math.max(1, (param.maxScale - param.minScale) / param.minSuccessful)

      @tailrec def loop(rand: Random, scale: Int, successful: Int = 0, discard: Int = 0): (Random, Property.Result) =
        if (checkTimeout(param)) (rand, Property.Timeout)
        else if (successful >= param.minSuccessful) (rand, Property.Pass)
        else if (discard >= param.maxDiscarded) (rand, Property.Discarded)
        else
          run(rand, scale) match {
            case (rand, Property.Pass) =>
              loop(rand, Math.min(param.maxScale, scale + scaleStep), successful + 1, 0)
            case (rand, Property.Discarded) =>
              loop(rand, Math.min(param.maxScale, scale + scaleStep), successful, discard + 1)
            case (rand, result) => (rand, result)
          }

      val future = Future(blocking {
        Property.startTime.withValue(System.currentTimeMillis())(loop(rand, param.minScale))
      })(ec)
      try Await.result(future, param.timeout)
      catch {
        case _: TimeoutException =>
          (rand.next, Property.Timeout)
      }
    }

  private def shrinkCounterExample[T](tree: Tree[Option[T]], param: Param, f: T => Boolean): Result = {
    @tailrec def loop(
        children: LazyList[Tree[Option[T]]],
        result: Result,
        shrink: Int = 0,
        discard: Int = 0,
        stack: List[(Int, LazyList[Tree[Option[T]]])] = List.empty
    ): Result =
      if (checkTimeout(param)) Timeout
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

  private val startTime: DynamicVariable[Long] = new DynamicVariable(0L)

  private def checkTimeout(param: Param): Boolean =
    System.currentTimeMillis() - startTime.value >= param.timeout.toMillis
}
