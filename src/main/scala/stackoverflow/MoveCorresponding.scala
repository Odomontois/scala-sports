/**
 * Author: Oleg Nizhnik
 * Date  : 29.07.2015
 * Time  : 17:05
 */
package stackoverflow

import purecsv.safe._
import shapeless.tag.Tagger
import scala.{util => ut}
import scalaz._
import Scalaz._
import spire.implicits._
import shapeless._
import shapeless.syntax.singleton._
import ops.hlist.{Selector, RightFolder}

object MoveCorresponding {


  case class Employee(empId: String,
                      designation: String,
                      age: Int,
                      salary: Long,
                      department: Int)
  case class Group(designation: String, department: Int)

  trait CorrespondingLow extends Poly2 {
    implicit def drop[E, L <: HList, L2 <: HList] = at[E, (L, Tagger[L2])] { case (_, (l, aux)) => (l, aux) }
  }
  object CorrespondingFolder extends CorrespondingLow {
    implicit def take[E, L <: HList, L2 <: HList]
    (implicit sel2: Selector[L2, E]) = at[E, (L, Tagger[L2])] { case (e, (l, aux)) => (e :: l, aux) }
  }
  class corresponding[R2] {
    def move[R1, L1 <: HList, L2 <: HList, L2A <: HList]
    (rec: R1)
    (implicit lgen1: LabelledGeneric.Aux[R1, L1],
     lgen2: LabelledGeneric.Aux[R2, L2],
     rf: RightFolder.Aux[L1, (HNil, Tagger[L2]), CorrespondingFolder.type, (L2A, Tagger[L2])],
     lgen2a: LabelledGeneric.Aux[R2, L2A]): R2 =
      lgen2a.from(lgen1.to(rec).foldRight((HNil: HNil, tag[L2]))(CorrespondingFolder)._1)
  }
  object corresponding {
    def apply[R2] = new corresponding[R2]
  }

  implicit class TryOps[T](t: ut.Try[T]) {
    def toValidation: ValidationNel[Throwable, T] = t match {
      case ut.Success(v) => v.success
      case ut.Failure(ex) => ex.failureNel
    }
  }
  def main(args: Array[String]) {
    val file = getClass.getResource("employees.txt").getFile

    val employees: ValidationNel[Throwable, Seq[Employee]] =
      CSVReader[Employee]
        .readCSVFromFileName(file)
        .traverseU(_.toValidation)
    //
    val averageSalary = (_: Seq[Employee])
      .groupBy(emp => corresponding[Group].move(emp))
      .mapValues {_
      .map(emp => BigDecimal(emp.salary))
      .qmean
    }
    employees foreach
      (averageSalary andThen (_ foreach println))
  }
}
