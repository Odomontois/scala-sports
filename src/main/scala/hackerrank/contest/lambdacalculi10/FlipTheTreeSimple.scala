/**
 * Author: Odomontois
 * Date  : 26-Apr-15
 * Time  : 20:48
 */
package hackerrank.contest.lambdacalculi10

object FlipTheTreeSimple extends App{

  import io.StdIn._

  val n = readInt()
  val parent = readLine() split " " map (_.toInt)
  val flip = Array.fill(n + 1)(false)

  def parents(x:Int):Stream[Int] = if (x == 1) Stream.empty else parent(x - 2) #:: parents(parent(x - 2))

  def common(x:Seq[Int], y:Seq[Int]) = {
    val u = y.toSet
    x.find(u).get
  }
  
  1 to readInt() foreach { _t => 
    val q = readLine split " " map (_.toInt)
    q match {
      case Array(1,x) => flip(x) = !flip(x)
      case Array(2, x ,y) => {
        val p = common(parents(x), parents(y))
        println(if ((x < y) ^ flip(p)) "L" else "R")
      }
    }
  }
}
