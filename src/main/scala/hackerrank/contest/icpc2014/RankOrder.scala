/**
 * Author: Oleg Nizhnik
 * Date  : 15.10.2015
 * Time  : 10:26
 */
package hackerrank.contest.icpc2014

object RankOrder extends App {
  import scala.io.Source
  import Numeric.Implicits._
  def order[T: Numeric](xs: Array[T]) = xs.view.zipWithIndex.sortBy(-_._1).map(_._2)
  val nums = Source.stdin.getLines().flatMap(_.split("\\s+").view.filter(!_.isEmpty).map(_.toInt))
  def cases(it: Iterator[Int]): Iterator[(Array[Int], Array[Int])] = if (it.isEmpty) Iterator.empty
  else {
    val n = it.next()
    val xs = it.take(n).toArray
    val ys = it.take(n).toArray
    Iterator.single((xs, ys)) ++ cases(it)
  }
  for(((xs, ys), cs) <- cases(nums).zipWithIndex) {
    val idx = order(xs).zip(order(ys)).indexWhere { case (x, y) => x != y }
    val result = if (idx >= 0) (idx + 1).toString else "agree"
    println(f"Case ${cs + 1}: $result")
  }
}