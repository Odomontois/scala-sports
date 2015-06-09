/**
 * Author: Oleg Nizhnik
 * Date  : 20.04.2015
 * Time  : 14:56
 */
package hackerrank.algorithm.number

object SherlockAndDivisors {
  val primes = 2 #:: (Stream.from(3, 2) filter isPrime)

  def isPrime(n: Int): Boolean = primes takeWhile (p => p * p <= n) find (n % _ == 0) isEmpty

  def factorize(n: Int, divs: Stream[Int] = primes): Stream[Int] = {
    val head #:: rest = divs
    if (n == 1) Stream.empty
    else if (head * head > n) Stream(n)
    else if (n % head == 0) head #:: factorize(n / head, divs)
    else factorize(n, rest)
  }

  def divisors(n: Int) = factorize(n).groupBy(identity).mapValues(_.size).toSeq.sorted

  def solution(n: Int) = divisors(n) match {
    case (2, k) +: odds => k * odds.map(_._2 + 1).product
    case _ => 0
  }

  def main(args: Array[String]) {
    1 to io.StdIn.readInt foreach (_t => println(solution(io.StdIn.readInt)))
  }
}
