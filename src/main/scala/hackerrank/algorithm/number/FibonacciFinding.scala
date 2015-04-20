/**
 * Author: Oleg Nizhnik
 * Date  : 20.04.2015
 * Time  : 15:17
 */
package hackerrank.algorithm.number


object FibonacciFinding extends App {
  import Integral.Implicits._
  type M[N] = ((N, N), (N, N))

  trait ModuloBase[N] {
    val mod: N
  }
  implicit object bigIntMod extends ModuloBase[BigInt] {
    val mod = BigInt(10).pow(9) + 7
  }
  implicit object longMod extends ModuloBase[Long] {
    val mod = bigIntMod.mod.toLong
  }

  implicit class Modulo[N](x: N)(implicit integral: Integral[N], base: ModuloBase[N]) {
    def *%(y: N) = x * y % base.mod
  }

  implicit class Matrix[N](u: M[N])(implicit integral: Integral[N], base: ModuloBase[N]) {
    import integral.{one, zero, fromInt}
    def unit: M[N] = ((one, zero), (zero, one))

    def *(v: M[N]) = (
      (u._1._1 *% v._1._1 + u._1._2 *% v._2._1, u._1._1 *% v._1._2 + u._1._2 *% v._2._2),
      (u._2._1 *% v._1._1 + u._2._2 *% v._2._1, u._2._1 *% v._1._2 + u._2._2 *% v._2._2))

    def **(pow: N): M[N] = if (pow == zero) unit
    else {
      val (div, mod) = pow /% fromInt(2)
      ((u * u) ** div) * (if (mod == one) u else unit)
    }
  }

  def solution[N](a: N, b: N, p: N)(implicit integral: Integral[N], base: ModuloBase[N]): N = {
    import integral.{one, zero}
    val fib = ((zero, one), (one, one))
    val first = ((a, b), (zero, zero))
    (first * (fib ** p))._1._1 % base.mod
  }

  1 to io.StdIn.readInt foreach { _t =>
    val Array(a,b,n) = io.StdIn.readLine split " " map (_.toLong)
    println(solution(a,b,n))
  }
}
