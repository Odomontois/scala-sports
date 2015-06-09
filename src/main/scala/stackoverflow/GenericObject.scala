/**
 * Author: Oleg Nizhnik
 * Date  : 08.06.2015
 * Time  : 13:02
 */
package stackoverflow

object GenericObject {

  trait Tree[+T] {
    def contains[U >: T : Ordering](num: U): Boolean

    def inc[U >: T : Ordering](num: U): Tree[U]
  }

  case object EmptyTree extends Tree[Nothing] {
    def contains[U >: Nothing : Ordering](num: U): Boolean = false
    def inc[U: Ordering](num: U): Tree[U] = DataTree(num, EmptyTree, EmptyTree)
    override def toString = "."
  }

  case class DataTree[T: Ordering](x: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    import Ordering.Implicits._
    def contains[U >: T : Ordering](num: U): Boolean =
      if (num < x) left.contains(x)
      else if (num > x) right.contains(x)
      else true

    def inc[U >: T : Ordering](num: U): Tree[U] =
      if (num < x) DataTree(x, left.inc(num), right)
      else if (num > x) DataTree(x, left, right.inc(num))
      else this

    override def toString = "{" + left + x + right + "}"
  }
}


