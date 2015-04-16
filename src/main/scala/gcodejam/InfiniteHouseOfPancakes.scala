/**
 * Author: Odomontois
 * Date  : 11-Apr-15
 * Time  : 18:51
 */
package gcodejam

import scala.language.{postfixOps, implicitConversions}
import scalaz.{Heap, _}
import Scalaz.listInstance

object InfiniteHouseOfPancakes extends CodeJamApp[Int] {
  implicit val reversedOrder: Order[Int] = Scalaz.intInstance.reverseOrder

  def split(cakes: Heap[Int]) = {
    val Some((head, rest)) = cakes.uncons
    rest + head / 2 + (head + 1) / 2
  }

  def timings(steps: Int = 0)(cakes: Heap[Int]): Stream[Int] = (cakes.minimum + steps) #::
    (if (cakes.minimum <= 3) Stream.Empty else timings(steps + 1)(split(cakes)))

  val parse: Solution[List[Int]] = for(_ <- getLine; line <- getLine) yield line split " " map (_.toInt) toList
  
  val result = Heap.fromData[List,Int] _ andThen timings() andThen (_.min)

  val solution = parse map result
}
