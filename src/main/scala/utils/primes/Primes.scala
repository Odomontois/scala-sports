/**
 * Author: Odomontois
 * Date  : 27-May-15
 * Time  : 10:11
 */
package utils.primes

import Integral.Implicits._
import Ordering.Implicits._

class Primes[T: Integral] {
  implicit val fromInt = implicitly[Numeric[T]].fromInt _
  val primes: Stream[T] = 2 #:: (Stream.from(3, 2) map fromInt filter isPrime)

  def isPrime(n: T): Boolean = !(primes takeWhile (p => p * p <= n) exists (n % _ == 0))

  def factorize(n: T, divs: Stream[T] = primes): Stream[T] = {
    val head #:: rest = divs
    if (n == 1) Stream.empty
    else if (head * head > n) Stream(n)
    else if (n % head == 0) head #:: factorize(n / head, divs)
    else factorize(n, rest)
  }
}
