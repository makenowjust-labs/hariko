package codes.quine.labo.hariko

import java.util.concurrent.TimeoutException

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.blocking
import scala.concurrent.duration.Duration
import scala.util.DynamicVariable
import scala.util.Failure
import scala.util.Try
import scala.util.control.NonFatal

import data.Tree
import random.Random
import util.Show

/**
  * Property is a property for testing.
  */
final case class Property(run: (Random, Param, ExecutionContext) => Property.Result) {

  /**
    * Runs this property on the implicit execution context.
    */
  def run(rand: Random, param: Param)(implicit ec: ExecutionContext): Property.Result = run(rand, param, ec)

  /**
    * Modifies parameter of this property.
    */
  def withParam(f: Param => Param): Property =
    Property((rand, param, ec) => run(rand, f(param), ec))
}

object Property {

  /**
    * Result is result of property execution.
    */
  sealed abstract class Result extends Serializable with Product {

    /**
      * Whether this result is `Pass` or not.
      */
    def isPass: Boolean = false
  }

  /**
    * Passes the propety.
    */
  final case class Pass(seed: Long, test: Int) extends Result {
    override def isPass: Boolean = true
    override def toString: String = s"pass (seed: 0x${seed.toHexString}, test: $test)"
  }

  /**
    * A counter example is found.
    */
  final case class CounterExample(seed: Long, test: Int, shrink: Int, value: Any) extends Result {
    override def toString: String =
      s"""counter example (seed: 0x${seed.toHexString}, test: $test, shrink: $shrink)
         |
         |value: ${Show.any(value)}
         |""".stripMargin
  }

  /**
    * No value is generated.
    */
  final case class NoValue(seed: Long, test: Int) extends Result {
    override def toString: String = s"no value (seed: 0x${seed.toHexString}, test: $test)"
  }

  /**
    * An error is occured in property execution.
    */
  final case class Error(seed: Long, test: Int, shrink: Int, value: Any, exception: Throwable) extends Result {
    override def toString: String =
      s"""error (seed: 0x${seed.toHexString}, test: $test, shrink: $shrink)
         |
         |value: ${Show.any(value)}
         |
         |exception: ${exception.toString}
         |""".stripMargin
  }

  /**
    * Times out property execution.
    */
  final case class Timeout(seed: Long, duration: Duration) extends Result {
    override def toString: String = s"timeout (seed 0x${seed.toHexString}, duration: $duration)"
  }

  /**
    * Checks property `f` on the generator.
    *
    * NOTE: it is utility method for testing in REPL.
    */
  def check[T](gen: Gen[T], param: Param = Param(System.currentTimeMillis()))(f: T => Boolean): Result = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val p = forAll(gen)(f)
    p.run(param.toRandom, param)
  }

  /**
    * Builds a property `f` on the generator.
    */
  def forAll[T](gen: Gen[T])(f: T => Boolean): Property =
    Property { (rand, param, ec0) =>
      implicit val ec = ec0

      def run(test: Int, rand0: Random, scale: Int): (Random, Property.Result) =
        if (checkTimeout(param)) (rand0, Timeout(param.seed, param.timeout))
        else {
          val (rand1, t) = gen.run(rand0, param, scale)
          if (checkTimeout(param)) (rand1, Timeout(param.seed, param.timeout))
          else
            try t.value match {
              case Some(x) =>
                if (f(x)) (rand1, Pass(param.seed, test))
                else
                  shrinkCounterExample(t, param)(f) match {
                    case Some((shrink, x)) => (rand1, CounterExample(param.seed, test, shrink, x))
                    case _                 => (rand1, Timeout(param.seed, param.timeout))
                  }
              case None => (rand1, NoValue(param.seed, test))
            } catch {
              case NonFatal(_) =>
                val u = t.map(_.map(x => (x, Try(f(x)))))
                shrinkCounterExample(u, param)(x => x._2.isSuccess) match {
                  case Some((shrink, (x, Failure(ex)))) => (rand1, Error(param.seed, test, shrink, x, ex))
                  case _                                => (rand1, Timeout(param.seed, param.timeout))
                }
            }
        }

      val scaleStep = Math.max(1, (param.maxScale - param.minScale) / param.minSuccessful)

      @tailrec def loop(rand: Random, scale: Int, successful: Int = 0, discard: Int = 0): Property.Result =
        if (checkTimeout(param)) Timeout(param.seed, param.timeout)
        else if (successful >= param.minSuccessful) Pass(param.seed, successful)
        else if (discard >= param.maxDiscarded) NoValue(param.seed, successful + 1)
        else
          run(successful + 1, rand, scale) match {
            case (rand, _: Pass) =>
              loop(rand, Math.min(param.maxScale, scale + scaleStep), successful + 1, 0)
            case (rand, _: NoValue) =>
              loop(rand, Math.min(param.maxScale, scale + scaleStep), successful, discard + 1)
            case (_, result) => result
          }

      val future = Future(blocking {
        Property.startTime.withValue(System.currentTimeMillis())(loop(rand, param.minScale))
      })
      // TODO: a thread still alives when propety is heavy. How do we shut down the thread?
      try Await.result(future, param.timeout)
      catch {
        case _: TimeoutException => Timeout(param.seed, param.timeout)
      }
    }

  /**
    * Shrinks a counter example from tree.
    */
  private def shrinkCounterExample[T](tree: Tree[Option[T]], param: Param)(f: T => Boolean): Option[(Int, T)] = {
    @tailrec def loop(
        children: LazyList[Tree[Option[T]]],
        x: T,
        shrink: Int = 0,
        discard: Int = 0,
        stack: List[(Int, LazyList[Tree[Option[T]]])] = List.empty
    ): Option[(Int, T)] =
      if (checkTimeout(param)) None
      else if (shrink >= param.maxShrink) Some((shrink, x))
      else if (discard >= param.maxDiscarded) stack match {
        case (d, ts) :: s => loop(ts, x, shrink, d, s)
        case Nil          => Some((shrink, x))
      }
      else
        children.headOption match {
          case Some(Tree(None, ts))             => loop(ts, x, shrink + 1, discard + 1, (discard, children.tail) :: stack)
          case Some(Tree(Some(x), ts)) if !f(x) => loop(ts, x, shrink + 1, discard, List.empty)
          case Some(_)                          => loop(children.tail, x, shrink + 1, discard, stack)
          case None                             => Some((shrink, x))
        }
    loop(tree.children, tree.value.get)
  }

  private val startTime: DynamicVariable[Long] = new DynamicVariable(0L)

  private def checkTimeout(param: Param): Boolean =
    System.currentTimeMillis() - startTime.value >= param.timeout.toMillis
}
