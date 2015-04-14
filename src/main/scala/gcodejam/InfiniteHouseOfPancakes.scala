/**
 * Author: Odomontois
 * Date  : 11-Apr-15
 * Time  : 18:51
 */
package gcodejam

import scalaz.Scalaz._
import scalaz.{Heap, _}

object InfiniteHouseOfPancakes extends CodeJamApp[Int] {

  implicit val maxOrder:Order[Int] = Order[Int].reverseOrder
  def split(cakes: Heap[Int]) = {
    val Some((head, rest)) = cakes.uncons
    rest.insert (head / 2)(maxOrder).insert((head + 1) / 2)(maxOrder)
  }
  
  def solutions(cakes: Heap[Int], steps: Int): Stream[Int] = (cakes.minimum + steps) #::
    (if (cakes.minimum <= 3) Stream.empty else solutions(split(cakes), steps + 1))
  
  val solution = for (_ <- getLine; line <- getLine) yield {
    val cakes = Heap.fromData[List, Int](line split " " map (_.toInt) toList)(maxOrder)
    solutions(cakes, 0).min
  }
}
