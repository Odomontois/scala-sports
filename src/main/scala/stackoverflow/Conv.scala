/**
 * Author: Oleg Nizhnik
 * Date  : 05.05.2015
 * Time  : 12:22
 */
package stackoverflow

object Conv {
  def main(args: Array[String]): Unit = {
    val xs = 1 to 1000000 map (_.toLong)

    println(xs.sum)
  }
}
