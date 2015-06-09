/**
 * Author: Odomontois
 * Date  : 25-Apr-15
 * Time  : 16:58
 */
package hackerrank.contest.lambdacalculi10

object CompositionOf2Primes extends App {
  val mod = (BigInt(10).pow(9) + 7).toLong
  val primes = 2 #:: (Stream.from(3, 2) filter isPrime)

  def isPrime(n: Int): Boolean = n > 2 && (primes takeWhile (p => p * p <= n) filter (n % _ == 0)).isEmpty
  val primeset = primes.takeWhile(_ < 1000000).toSet

  def split(n: Int): Seq[Int] = n match {
    case n if n % 2 == 0 => primes.takeWhile(_ < n / 2).find(p => primeset(p) && primeset(n - p)).toSeq.flatMap(p => Seq(p, n - p))
    case n if primeset(n - 2) => Seq(2, n - 2)
    case _ => Seq.empty
  }

  val sums = 1 to 1000000 map split

  1 to io.StdIn.readInt foreach { _t =>
    val Array(a, b) = io.StdIn.readLine split " " map (_.toInt)
    val all = sums.slice(a - 1, b)
    val count = all.count(_.nonEmpty)
    val product = all.iterator.flatten map (_.toLong) reduce (_ * _ % mod)
    println(f"$count $product")
  }
}
