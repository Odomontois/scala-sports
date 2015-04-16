/**
 * Author: Oleg Nizhnik
 * Date  : 14.04.2015
 * Time  : 16:01
 */

import scala.io.StdIn

object Round298D2A extends App {
  val studs = StdIn.readInt() match {
    case 1 | 2 => List(1)
    case 3 => List(1, 3)
    case 4 => List(3, 1, 4, 2)
    case n if n % 2 == 1 => 1 to 2 * n by 2 map (_ % n + 1)
    case n => (1 to n by 2) ++ (2 to n by 2)
  }
  println(studs.length)
  println(studs mkString " ")
}
