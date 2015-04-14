/**
 * Author: Odomontois
 * Date  : 11-Apr-15
 * Time  : 18:51
 */
package gcodejam

import utils.{CodeJamMonadicApp, CodeJamApp}

import scala.collection.{mutable, SortedSet}

object InfiniteHouseOfPancakes extends CodeJamMonadicApp[Int] {
  
  def split(cakes: mutable.PriorityQueue[Int]) = {
    val head = cakes.dequeue()
    cakes += (head / 2) += ((head + 1) / 2)
  }
  
  def solutions(cakes: mutable.PriorityQueue[Int], steps: Int): Stream[Int] = (cakes.head + steps) #::
    (if (cakes.head <= 3) Stream.empty else solutions(split(cakes), steps + 1))
  
  val solution = for(_ <- getLine; line <- getLine) yield {
    val cakes = mutable.PriorityQueue(line split " " map (_.toInt): _*)
    solutions(cakes, 0).min
  }
}
