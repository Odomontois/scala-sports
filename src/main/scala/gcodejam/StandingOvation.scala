/**
 * Author: Odomontois
 * Date  : 11-Apr-15
 * Time  : 11:01
 */
package gcodejam

import gcodejam.CodeJamApp

object StandingOvation extends CodeJamApp[Int] {
  def friends(standing: Int, aggr: Int, auds: Seq[Int], level: Int): Int =
    if (auds.isEmpty) aggr
    else {
      val add = if (level > standing) 1 else 0
      friends(standing + auds.head + add, aggr + add, auds.tail, level + 1)
    }

  val solution = for (line <- getLine) yield {
    val auds = (line split " " tail).last map (_.toInt - '0'.toInt)
    friends(0, 0, auds, 0)
  }
}
