/**
 * Author: Oleg Nizhnik
 * Date  : 20.04.2015
 * Time  : 11:52
 */
import scala.language.postfixOps

object X extends App{
  var str = "123.4"
  var d = str toDouble

  if (d > 10)  println("Larger than 10")

  for{i <- 1 to 4
      x = i + 4
      j <- 1 until x} println(f"$j < $x")
}