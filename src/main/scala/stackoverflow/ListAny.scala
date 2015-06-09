/**
 * Author: Odomontois
 * Date  : 20-Apr-15
 * Time  : 21:10
 */
package stackoverflow

import scala.reflect.runtime.universe._
object ListAny {
  val e: Either[String, Int] = Right(100)
  val o: Option[Int] = None
  val u = List(e, o, 1)

}
