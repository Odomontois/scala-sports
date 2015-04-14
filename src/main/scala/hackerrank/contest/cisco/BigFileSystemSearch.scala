/**
 * Author: Odomontois
 * Date  : 09-Apr-15
 * Time  : 18:34
 */
package hackerrank.contest.cisco

import scala.collection.immutable.SortedSet
import scala.io.StdIn._
import scala.language.postfixOps

object Solution extends App {
  type Reducer[X] = (X, X) => X
  type Pairs = Iterable[(Int, Int)]

  def frequencies(seq: Seq[Int]): Pairs = seq groupBy identity mapValues (_.size)

  def uniques(seq: Seq[Int]): Pairs = seq.toSet map ((_:Int, 1))

  val files: Map[Int, SortedSet[(Int, Int)]] =
    (for (i <- 1 to readInt;
          (el, cnt) <- frequencies((readLine split " ").tail map (_.toInt))
    ) yield (el, (cnt, i))) groupBy (_._1) mapValues (is => SortedSet(is map (_._2): _*))

  val queries: Seq[Query] = (1 to readInt).toStream map (_i => (readLine split " ").toSeq map (_.toInt) match {
    case 1 +: _ +: nums => All(nums)
    case 2 +: _ +: nums => Any(nums)
    case 3 +: _ +: nums => Some(nums)
  })

  def contains(num: Int, count: Int) = files(num) from(count, 0) map (_._2)

  def query(pairs: Pairs, reducer: Reducer[Set[Int]]) = pairs map (contains _).tupled reduce reducer size

  trait Query {
    val nums: Seq[Int]

    def query(mapper: Seq[Int] => Pairs, reducer: Reducer[Set[Int]]) = mapper(nums) map (contains _).tupled reduce reducer size

    def calc: Int
  }

  case class All(nums: Seq[Int]) extends Query {
    def calc = query(frequencies, _ intersect _)
  }

  case class Any(nums: Seq[Int]) extends Query {
    def calc = query(uniques,_ union _)
  }

  case class Some(nums: Seq[Int]) extends Query {
    def calc = query(uniques,_ union _) - query(frequencies,_ intersect _)
  }

  queries map (_.calc) foreach println
}
