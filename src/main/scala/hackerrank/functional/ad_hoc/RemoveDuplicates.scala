/**
 * Author: Oleg Nizhnik
 * Date  : 15.05.2015
 * Time  : 15:51
 */
package hackerrank.functional.ad_hoc
import scala.collection.BitSet

object RemoveDuplicates extends App {
  println(((BitSet.empty, StringBuilder.newBuilder) /: io.StdIn.readLine) { (step, char) =>
    val (set, bld) = step
    if (set(char)) (set, bld) else (set + char, bld + char)
  }._2.mkString)
}
