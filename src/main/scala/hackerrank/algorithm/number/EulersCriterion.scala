/**
 * Author: Oleg Nizhnik
 * Date  : 20.04.2015
 * Time  : 14:42
 */
package hackerrank.algorithm.number

object EulersCriterion extends App {
  case object YES
  case object NO
  1 to io.StdIn.readInt foreach { _t =>
    val Array(a, m) = io.StdIn.readLine split " " map BigInt.apply
    println(if (a % m == 0 || a.modPow((m - 1) / 2, m) == 1) YES else NO)
  }
}
