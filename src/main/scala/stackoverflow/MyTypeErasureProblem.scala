/**
 * Author: Oleg Nizhnik
 * Date  : 20.04.2015
 * Time  : 9:18
 */
package stackoverflow

import scala.language.existentials
import scala.reflect.runtime.universe._

trait Holders

case class MapHolder[T](f: T => T)(implicit tag: TypeTag[T]) {
  def buildWorker() = MapWrk[T](f)
}

trait Worker

class MapWrk[T](val f: T => T)(implicit val tag: TypeTag[T]) extends Worker

object MapWrk {

  def apply[T](f: T => T)(implicit tpy: TypeTag[T]) = new MapWrk[T](f)

  def unapply(worker: Worker): Option[(_ => _, Type)] = worker match {
    case mapWrk: MapWrk[_] => Some((mapWrk.f, mapWrk.tag.tpe))
    case _ => None
  }
}

object MyTypeErasureProblem extends App {
  val pipeline = List(
    MapHolder[Int]((x: Int) => x + 10).buildWorker(),
    MapHolder[String]((x: String) => x + "abc").buildWorker()
  )

  val result = pipeline collect {
    case MapWrk(f, tpe) if tpe =:= typeOf[Int] => "Int endofunction"
    case MapWrk(f, tpe) if tpe =:= typeOf[String] => "String endofunction"
  }

  result foreach println

}
