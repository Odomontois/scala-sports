/**
 * Author: Odomontois
 * Date  : 28-Apr-15
 * Time  : 19:33
 */
package stackoverflow.recmap

import java.text.SimpleDateFormat
import java.util.Date

object  RecMap {
  import java.text.SimpleDateFormat
  import java.util.Date


  sealed trait RecMap[K, V]

  case class MapUnit[K, V](elem: V) extends RecMap[K, V] {
    override def toString = elem.toString
  }
  case class MapLayer[K, V](map: Map[K, RecMap[K, V]]) extends RecMap[K, V] {
    override def toString = map.toString()
  }

  implicit class GroupOps[A](coll: Seq[A]) {
    def groupByKeys[B](fs: (A => B)*): Map[Seq[B], Seq[A]] =
      coll.groupBy(elem => fs map (_(elem)))

    def groupRecursive[B](fs: (A => B)*): RecMap[B, Seq[A]] = fs match {
      case Seq() => MapUnit(coll)
      case f +: fs => MapLayer(coll groupBy f mapValues {_.groupRecursive(fs: _*)})
    }
  }


  case class User(name: String, city: String, birthDate: Date) {
    override def toString = name

  }

  implicit val date = new SimpleDateFormat("yyyy-MM-dd").parse(_: String)

  val month = new SimpleDateFormat("MMM").format(_: Date)

  val users = List(
    User(name = "mrhaki", city = "Tilburg", birthDate = "1973-9-7"),
    User(name = "bob", city = "New York", birthDate = "1963-3-30"),
    User(name = "britt", city = "Amsterdam", birthDate = "1980-5-12"),
    User(name = "kim", city = "Amsterdam", birthDate = "1983-3-30"),
    User(name = "liam", city = "Tilburg", birthDate = "2009-5-6")
  )

  users.groupRecursive(_.city, u => month(u.birthDate))

  val a = 1 to 20

  a.groupByKeys(_ % 3, _ % 2) foreach println
  val x = a.groupRecursive(_ % 3, _ % 2)
  x.toString
}


