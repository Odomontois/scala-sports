import shapeless._

import scala.reflect.runtime.universe._
trait HListEv[-L <: HList, +E] {
  def tag: String
}
implicit object HNilEv extends HListEv[HNil, Nothing] {
  def tag = "[]"
}
class HConsEv[-L <: HList, +E: TypeTag] extends HListEv[L, E] {
  def tag = typeTag[E].tpe.toString
}

implicit def hConsEv[E: TypeTag, T <: HList](implicit tailEv: HListEv[T, E]): HConsEv[E :: T, E] = new HConsEv[E :: T, E]

def elem[L <: HList, E: TypeTag](list: L)(implicit ev: HListEv[L, E]) = ev

sealed trait Ternary {
  type S <: Ternary
  def next(implicit s: S) = s
}
sealed trait One extends Ternary {
  type S = Two
}
implicit object One extends One
sealed trait Two extends Ternary {
  type S = Zero
}
implicit object Two extends Two
sealed trait Zero extends Ternary {
  type S = One
}
implicit object Zero extends Zero
object sum extends Poly2 {
  implicit def atNumeric[N: Numeric] = at[Int, N]((acc, _) => acc + 1)
  implicit def atString = at[Int, String]((acc, _) => acc)
 }

(1 :: 2.0 :: "lolo" :: BigInt(3) :: HNil).foldLeft(0)(sum)





