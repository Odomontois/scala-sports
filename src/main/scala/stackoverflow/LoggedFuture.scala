/**
 * Author: Oleg Nizhnik
 * Date  : 12.05.2015
 * Time  : 15:58
 */
package stackoverflow
import scala.concurrent.{Await, Future}
import scalaz._
import Scalaz._
import WriterT._
import Numeric.Implicits._
import concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._

object LoggedFuture extends App {
  type Logged[T] = WriterT[Future, Stream[String], T]

  implicit class StringLogOps(s: => String) {
    def log = writerT(Future((Stream(s), ())))
  }

  def mul[N](x: N, q: N)(implicit numeric: Numeric[N]): Logged[N] = for {
    _ <- f"Multiplying $x by $q".log
  } yield x * q

  implicit def futLogNumeric[N](implicit num: Numeric[N]): Numeric[Logged[N]] = new Numeric[Logged[N]] {
    def run[T](x: Logged[T]) = Await.result(x.run, Duration.Inf)._2
    override def plus(x: Logged[N], y: Logged[N]): Logged[N] = for {
      xx <- x
      yy <- y
      _ <- f"adding $xx to $yy".log
    } yield xx + yy
    override def toDouble(x: Logged[N]): Double = run(x).toDouble
    override def toFloat(x: Logged[N]): Float = run(x).toFloat
    override def toInt(x: Logged[N]): Int = run(x).toInt
    override def negate(x: Logged[N]): Logged[N] = x map (-_)
    override def fromInt(x: Int): Logged[N] = x.point[Logged] map num.fromInt
    override def toLong(x: Logged[N]): Long = run(x).toLong
    override def times(x: Logged[N], y: Logged[N]): Logged[N] = for {
      xx <- x
      yy <- y
      _ <- f"multiplying $xx by $yy".log
    } yield xx * yy
    override def minus(x: Logged[N], y: Logged[N]): Logged[N] = for {
      xx <- x
      yy <- y
      _ <- f"subtracting $yy from $xx".log
    } yield xx - yy
    override def compare(x: Logged[N], y: Logged[N]): Int = run(for {
      xx <- x
      yy <- y
      _ <- "comparing $xx and $yy".log
    } yield num.compare(xx, yy))
  }

  val xs = 1 to 1000 map (_.point[Logged]) sum

  Await.ready(xs.written.map(_ foreach println), Duration.Inf)
}
