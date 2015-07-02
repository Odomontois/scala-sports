/**
 * Author: Oleg Nizhnik
 * Date  : 02.07.2015
 * Time  : 12:57
 */
package stackoverflow

object Shapeless {
  import shapeless._

  object diff {
    class Differ[T <: HList](val diff: (T, T) => List[String])

    def apply[T <: HList](l1: T, l2: T)(implicit differ: Differ[T]): List[String] = differ.diff(l1, l2)

    implicit object NilDiff extends Differ[HNil]((_, _) => Nil)

    implicit def ConsDiff[H, T <: HList : Differ] = new Differ[H :: T]({
      case (h1 :: t1, h2 :: t2) if h1 != h2 => s"$h1 -> $h2" :: diff(t1, t2)
      case (h1 :: t1, h2 :: t2) => diff(t1, t2)
    })
  }

  def main(args: Array[String]) {
    println(diff(1 :: 2 :: HNil, 2 :: 3 :: HNil))
  }
  //def diff[H[X] <: HList, T](lst1: H[T], lst2:H[T]):List[String] = (lst1, lst2) match {
  //  case (HNil, HNil)                 => List()
  //  case (h1::t1, h2::t2) if h1 != h2 => s"$h1 -> $h2" :: diff(t1, t2)
  //  case (h1::t1, h2::t2)             => diff(t1, t2)
  //  case _                            => throw new RuntimeException("something went very wrong")
  //}

  //object diff extends Poly2{
  //  implicit def caseNil = at[HNil, HNil]((_:HNil, _:HNil) => List())
  //  implicit def caseCons[Head,Tail] = at[Head::Tail, Head::Tail]((l1:Head::Tail, l2:Head::Tail) => (l1,l2) match {
  //    case (h1::t1, h2::t2) if h1 != h2 => s"$h1 -> $h2" :: diff(t1,t2).asInstanceOf[List[String]]
  //   }
  //}

}
