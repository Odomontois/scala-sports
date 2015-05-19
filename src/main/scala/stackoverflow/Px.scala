/**
 * Author: Oleg Nizhnik
 * Date  : 24.04.2015
 * Time  : 14:07
 */
package stackoverflow

sealed abstract class Px[A] {

  /** Revision of its value. Increments when value changes. */
  def rev: Int

  /** Get the latest value, updating itself if necessary. */
  def value(): A

  /** Get the last used value without updating. */
  def peek: A

  def freeze = new FrozenPx(rev, this, value())

  final def valueSince(r: Int): Option[A] = {
    val v = value() // Ensure value and rev are up-to-date
    if (rev != r)
      Some(v)
    else
      None
  }
  // override def toString = value().toString
}

class FrozenPx[A](val rev: Int, val px: Px[A], val value: A) {
  override def equals(o: Any) = o match {
    case that: FrozenPx[A] => rev == that.rev && px == that.px
    case _ => false
  }
}
