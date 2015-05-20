/**
 * Author: Oleg Nizhnik
 * Date  : 19.05.2015
 * Time  : 15:23
 */
package stackoverflow
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.util.{Success, Try}

object Convert extends App {
  trait Result
  case class Ok(mess: String = "ok") extends Result
  case class BadRequest(mess: String = "bad") extends Result
  def convert[T](x: Option[String], f: String => T) = x map (v => Try(Some(f(v)))) getOrElse Success(None)
  def sequence[T](xs: Seq[Try[T]]): Try[Seq[T]] = (Try(Vector[T]()) /: xs) { (tseq, tx) => for {
    seq <- tseq
    x <- tx
  } yield seq :+ x
  }
  val booleans = sequence("false true true false" split ' ' map (s => Try(s.toBoolean)))
  val ints = sequence("1 2 3 4 5" split ' ' map (s => Try(s.toInt)))
  val handleBools = 1 to 4 map (i => (b: Boolean) => println(f"boolean #$i is $b"))
  val handleInts = 1 to 5 map (i => (x: Int) => println(f"integer #$i is $x"))

  (for {
    bools <- booleans
    ints <- ints
  } yield {
      (handleBools, bools).zipped map (_(_))
      (handleInts, ints).zipped map (_(_))
    }) map (_ => Ok()) recover { case ex => BadRequest(ex.getMessage) } get

}
