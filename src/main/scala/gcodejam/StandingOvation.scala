/**
 * Author: Odomontois
 * Date  : 11-Apr-15
 * Time  : 11:01
 */
package gcodejam

object StandingOvation extends CodeJamApp[Int] {
  def friends(standing: Int = 0, aggr: Int = 0, level: Int = 0)(auds: Seq[Int]): Int =
    if (auds.isEmpty) aggr
    else {
      val add = if (level > standing) 1 else 0
      friends(standing + auds.head + add, aggr + add, level + 1)(auds.tail)
    }

  val parse: Solution[Seq[Int]] = for(line <- getLine) yield (line split " ").last map (_.toInt - '0'.toInt)

  val solution = parse map friends()
}
