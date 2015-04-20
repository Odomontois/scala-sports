/**
 * Author: Oleg Nizhnik
 * Date  : 20.04.2015
 * Time  : 13:11
 */
package hackerrank.algorithm.number

import io.StdIn._
object PossiblePath extends App {
  case object YES
  case object NO
  1 to readInt() foreach { _t =>
    val Array(a, b, x, y) = readLine split " " map BigInt.apply
    println(if ((a gcd b) == (x gcd y)) YES else NO)
  }
}
