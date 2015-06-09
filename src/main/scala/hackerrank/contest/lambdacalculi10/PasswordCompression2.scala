/**
 * Author: Odomontois
 * Date  : 25-Apr-15
 * Time  : 16:48
 */
package hackerrank.contest.lambdacalculi10

object PasswordCompression2 extends App{
  val token = "(\\d+)(\\w+)".r
  println(io.StdIn.readLine split " " map { case token(idx,wrd) => wrd.applyOrElse(idx.toInt,(_:Int) => ' ') } mkString)
  
}
