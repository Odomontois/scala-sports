/**
 * Author: Oleg Nizhnik
 * Date  : 09.06.2015
 * Time  : 15:56
 */
package codechef.bogosort

import Integral.Implicits._
import Ordering.Implicits._
import Fractional.Implicits._
import scala.collection.{IterableLike, SeqLike, TraversableLike}
import scala.language.{implicitConversions}
object Main {

  implicit class ItOps[A](val s1: Iterable[A]) {
    import Numeric.Implicits._
    def @#[B, C](s2: Iterable[B])(f: (A, B) => C) = s1 zip s2 map { case (x1, x2) => f(x1, x2) }
    def *#(s2: Iterable[A])(implicit n: Numeric[A]) = @#(s2)(_ * _)
    def *#+(s2: Iterable[A])(implicit n: Numeric[A]) = s1 *# s2 sum
    def +#(s2: Iterable[A])(implicit n: Numeric[A]) = @#(s2)(_ + _)
  }

  class Solution[N: Integral] {
    class Rat private[Rat](val num: N, val denom: N) {
      override def toString = if (denom == 1) f"$num" else f"$num/$denom"
    }
    implicit object Rat extends Fractional[Rat] {
      val N = implicitly[Integral[N]]
      def gcd(a: N, b: N): N = if (b > a) gcd(b, a) else if (b == N.zero) a else gcd(b, a % b)

      def apply(num: N, denom: N) = {
        val g = gcd(num, denom)
        new Rat(num / g, denom / g)
      }

      override def div(x: Rat, y: Rat): Rat = Rat(x.num * y.denom, x.denom * y.num)
      override def toDouble(x: Rat): Double = x.num.toDouble() / x.denom.toDouble()
      override def plus(x: Rat, y: Rat): Rat = Rat(x.num * y.denom + y.num * x.denom, x.denom * y.denom)
      override def toFloat(x: Rat): Float = x.num.toFloat() / x.denom.toFloat()
      override def toInt(x: Rat): Int = (x.num / x.denom).toInt()
      override def negate(x: Rat): Rat = Rat(-x.num, x.denom)
      override def fromInt(x: Int): Rat = Rat(N.fromInt(x), N.one)
      override def toLong(x: Rat): Long = (x.num / x.denom).toLong()
      override def times(x: Rat, y: Rat): Rat = Rat(x.num * y.num, x.denom * y.denom)
      override def minus(x: Rat, y: Rat): Rat = Rat(x.num * y.denom - y.num * x.denom, x.denom * y.denom)
      override def compare(x: Rat, y: Rat): Int = N.compare(x.num * y.denom, x.denom * y.num)
    }
    import Rat.{one, zero, fromInt}

    val fact = (Stream from 1 map fromInt scan one)(_ * _)
    val comb = Stream.iterate(Vector(one))(line => (line :+ zero) +# (zero +: line) toVector)
    val displaced: Stream[Rat] = one #:: zero #:: (fact.drop(2) @# comb.drop(2))(_ - _.init *#+ displaced).toStream
    val result: Stream[Rat] = zero #:: zero #:: (fact.drop(2) zip Stream.from(2) zip displaced.drop(2) map {
      case ((f_n, n), u_n) =>
        val ks = 1 until n map (n + 1 - _) map fromInt
        val reducing = ks *# displaced.tail
        println(f_n, u_n, reducing, n)
        (f_n + reducing *#+ result.tail) / (reducing.sum + one)
    })
  }

  def main(args: Array[String]) {
    val c = new Solution[BigInt]
    c.displaced.take(10).foreach(println)
    c.fact.take(10).foreach(println)
    c.comb.take(10).foreach(println)
    c.result.zip(Stream from 0 take 10).foreach(println)
  }
}
