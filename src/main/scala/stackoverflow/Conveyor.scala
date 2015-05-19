/**
 * Author: Oleg Nizhnik
 * Date  : 05.05.2015
 * Time  : 10:27
 */
package stackoverflow
import scala.language.higherKinds

case class Conveyor[E, +A](take: Stream[E] => (Stream[A], Stream[E])) {
  def map[B](f: A => B): Conveyor[E, B] = Conveyor(take andThen { case (x, s) => (x map f, s) })

  def flatMap[B](f: A => Conveyor[E, B]): Conveyor[E, B] = Conveyor { stream =>
    val (xs, next) = take(stream)
    xs.foldLeft((Stream.empty[B], next)) {
      case ((res, stream), x) =>
        val (ys, next) = f(x).take(stream)
        (res #::: ys, next)
    }
  }

  def run(stream: Stream[E]): Stream[A] = if (stream.isEmpty) Stream.empty
  else {
    val (xs, next) = take(stream)
    xs #::: run(next)
  }

  def filter(f: A => Boolean): Conveyor[E, A] = Conveyor { stream =>
    val (xs, next) = take(stream)
    (xs filter f, next)
  }
}

object Conveyor {
  def push[E, A](xs: A*): Conveyor[E, A] = pushAll(xs)
  def pushAll[E, A](xs: TraversableOnce[A]): Conveyor[E, A] = Conveyor { s => (xs.toStream, s) }
  def pull[E]: Conveyor[E, E] = buffer(1)
  def buffer[E](k: Int): Conveyor[E, E] = Conveyor {_.splitAt(k)}
  def pullSeq[E](k: Int): Conveyor[E, Stream[E]] = Conveyor { stream =>
    (Stream(stream take k), stream drop k)
  }
  def pick[E]: Conveyor[E, E] = Conveyor {
    case stream@Stream(head, _*) => (Stream(head), stream)
    case Stream() => (Stream.empty, Stream.empty)
  }
  def eval[E, A](f: Stream[E] => A): Conveyor[E, A] = Conveyor { case s => (Stream(f(s)), s) }
}

