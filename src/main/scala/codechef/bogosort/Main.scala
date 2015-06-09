/**
 * Author: Oleg Nizhnik
 * Date  : 09.06.2015
 * Time  : 15:56
 */
package codechef.bogosort
//import Numeric.Implicits._
import Integral.Implicits._
import Ordering.Implicits._
import scala.language.implicitConversions
object Main {
  class Combin[N: Integral] {
    val n = implicitly[Integral[N]]
    import n.{one, zero, fromInt}
    def gcd(a: N, b: N): N = if (b > a) gcd(b, a) else if (b == zero) a else gcd(b, a % b)
    class Rat private[Rat](val num: N, val denom: N) {
      def +(o: Rat) = Rat(num * o.denom + o.num * denom, denom * o.denom)
      def *(o: Rat) = Rat(num * o.num, denom * o.denom)
      def /(o: Rat) = Rat(num * o.denom, denom * o.num)
      def -(o: Rat) = Rat(num - o.denom, denom * o.num)
      override def toString = f"$num/$denom"
    }
    object Rat {
      def apply(num: N, denom: N) = {
        val g = gcd(num, denom)
        new Rat(num / g, denom / g)
      }
    }
    implicit class ToRat(n: N) {
      def rat = Rat(n, one)
    }

    val fact = Stream.from(1).map(fromInt).scan(one)(_ * _)
    val comb = Stream.iterate(Vector(one)) { line =>
      (zero +: line) zip (line :+ zero) map { case (a, b) => a + b }
    }
    val displaced: Stream[N] = one #:: zero #:: (comb.drop(2) zip fact.drop(2) map {
      case (c_n, f_n) => f_n - (c_n.init zip displaced map { case (a, b) => a * b }).sum
    })
    val result: Stream[Rat] = zero.rat #:: (fact.tail zip comb.tail zip displaced.tail map {
      case ((f_n, c_n), u_n) =>
        (f_n.rat + (c_n zip displaced zip result map {
          case ((c, u), x) => c.rat * u.rat * x
        } reduce (_ + _))) / (f_n.rat - u_n.rat)
    })
  }

  def main(args: Array[String]) {
    val c = new Combin[Long]
    c.displaced.take(10).foreach(println)
    c.fact.take(10).foreach(println)
    c.comb.take(10).foreach(println)
    c.result.take(10).foreach(println)
  }
}
