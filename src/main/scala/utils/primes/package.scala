/**
 * Author: Odomontois
 * Date  : 27-May-15
 * Time  : 10:10
 */
package utils

import Numeric.Implicits._

package object primes {

  import utils.primes.Primes

  implicit class Primes2[T: Integral](val p: Primes[T]) {

    import p.fromInt

    val one: T = 1

    def has4divs(n: T) = p.factorize(n) match {
      case Stream(p, q) if p != q => true
      case Stream(p, q, w) if p == q && q == w => true
      case _ => false
    }

    val triplets = p.primes map (_ * 2) filter (n => has4divs(n - 1) && has4divs(n + one))
  }
}
