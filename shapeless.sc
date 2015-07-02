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

trait Ternary
trait TernaryNonZero extends Ternary
case object One extends TernaryNonZero
case object Two extends TernaryNonZero
case object Zero extends Ternary

object sum extends Poly2 {
  implicit def atZeroes = at[Zero.type, Zero.type]((_, _) => Zero)
  implicit def atLeftZero[T <: TernaryNonZero] = at[T, Zero.type]((x, _) => x)
  implicit def atRightZero[T <: TernaryNonZero] = at[Zero.type, T]((_, x) => x)
  implicit def atOneTwo = at[One.type, Two.type]((_, _) => Zero)
  implicit def atTwoOne = at[Two.type, One.type]((_, _) => Zero)
  implicit def atOneOne = at[One.type, One.type]((_, _) => Two)
  implicit def atTwoTwo = at[Two.type, Two.type]((_, _) => One)
}

(One :: Two :: Two :: One :: HNil).foldLeft(Zero)(sum)





