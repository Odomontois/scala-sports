/**
 * Author: Oleg Nizhnik
 * Date  : 20.04.2015
 * Time  : 14:17
 */
package hackerrank.algorithm.number

object Restaraunt extends App{
  1 to readInt foreach { _t =>
    val Array(x,y) = readLine split " " map BigInt.apply
    println(x * y / (x gcd y pow 2) )
  }
}
