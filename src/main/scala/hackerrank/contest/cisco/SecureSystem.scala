/**
 * Author: Odomontois
 * Date  : 09-Apr-15
 * Time  : 18:04
 */
package hackerrank.contest.cisco
import io.StdIn._
object SecureSystem extends App {
    1 to readInt foreach  { _t => println(
      readLine split " " map (_.toInt) match {
        case Array(mi,ma) if ma > 6 => "YES"
        case Array(mi,6) if mi < 6 => "YES"
        case _ => "NO"
      })
    }
}
