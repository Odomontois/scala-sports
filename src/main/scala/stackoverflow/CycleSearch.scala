/**
 * Author: Odomontois
 * Date  : 14-May-15
 * Time  : 22:15
 */
package stackoverflow

object CycleSearch {
  def neum(a: Int): Iterator[Int] = Iterator.iterate(a)(a => (a * a / 100) % 10000)

  def cycleOf[T](seq: => Iterator[T]): (Iterator[T], Iterator[T]) = {
    def fast = seq.sliding(1, 2) map (_.head)
    val meet = seq zip fast drop 1 dropWhile { case (x, y) => x != y }
    val met = meet.next()
    val period = (meet indexOf met) + 1
    val forward = seq drop period
    val (prefix, rest) = seq span {_ != forward.next()}
    val cycle = rest take period
    (prefix, cycle)
  }
}
