/**
 * Author: Oleg Nizhnik
 * Date  : 14.04.2015
 * Time  : 16:29
 */
package Codeforces.Round298d2

import scala.io.StdIn

object Round298D2B extends App {
  def pair = StdIn.readLine() split " " map (_.toInt)

  val Array(v1, v2) = pair
  val Array(t, d) = pair
  println(if (v1 == v2) v1 * t
  else {
    val tx = (t + (v2 - v1) / d) / 2
  })
}
