/**
 * Author: Oleg Nizhnik
 * Date  : 15.05.2015
 * Time  : 16:19
 */
package hackerrank.functional.ad_hoc
//import Numeric.Implicits._
import Integral.Implicits._

object HugeGCD {
  type Fact = IndexedSeq[(Int, Int)]
  val primes = 2 #:: (Stream.from(3, 2) filter isPrime)
  def primesTo(n: Int) = primes takeWhile (p => p * p <= n)
  def isPrime(n: Int): Boolean = !primesTo(n).exists(n % _ == 0)
  def factorize(n: Int, facts: Fact = IndexedSeq.empty, ps: Stream[Int] = primes): Fact =
    if (n == 1) facts
    else {
      def next(p: Int) = facts match {
        case prev :+ ((`p`, cnt)) => prev :+(p, cnt + 1)
        case _ => facts :+(p, 1)
      }
      val p #:: other = ps

      if (p * p > n) next(n)
      else if (n % p != 0) factorize(n, facts, other)
      else factorize(n / p, next(p), ps)
    }
  def times(a: Fact, b: Fact, acc: Fact = IndexedSeq()): Fact = a match {
    case Seq() => acc ++ b
    case ((pa, ca)) +: ta => b match {
      case Seq() => acc ++ a
      case ((`pa`, cb)) +: tb => times(ta, tb, acc :+ ((pa, ca + cb)))
      case ((pb, cb)) +: tb if pb < pa => times(a, tb, acc :+ ((pb, cb)))
      case _ => times(ta, b, acc :+ ((pa, ca)))
    }
  }

  def fgcd(a: Fact, b: Fact, acc: Fact = IndexedSeq()): Fact = a match {
    case Seq() => acc
    case ((pa, ca)) +: ta => b match {
      case Seq() => acc
      case ((`pa`, cb)) +: tb => fgcd(ta, tb, acc :+ ((pa, math.min(ca, cb))))
      case ((pb, cb)) +: tb if pb < pa => fgcd(a, tb, acc)
      case _ => fgcd(ta, b, acc)
    }
  }
  implicit class CalcInt(fact: Fact) {
    implicit def fromInt[N](x: Int)(implicit num: Numeric[N]): N = num fromInt x
    def toNum[N: Numeric]: N = fact.iterator.flatMap { case (p, k) => Iterator.fill[N](k)(p) }.product

    def toNumM[N: Integral](modulo: N): N =
      fact.iterator.flatMap { case (p, k) => Iterator.fill[N](k)(p) }.foldLeft[N](1)(_ * _ % modulo)

    def *~(that: Fact) = times(fact, that)

    def gcd(that: Fact) = fgcd(fact, that)
  }

  def main(args: Array[String]): Unit = {
    def readNum = {
      io.StdIn.readLine()
      io.StdIn.readLine split " " map (_.toInt) map (factorize(_)) reduce (_ *~ _)
    }
    println(readNum gcd readNum toNumM 1000000007L)
  }
}
