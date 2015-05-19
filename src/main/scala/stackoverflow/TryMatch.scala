/**
 * Author: Oleg Nizhnik
 * Date  : 15.05.2015
 * Time  : 12:35
 */
package stackoverflow
import java.io.FileNotFoundException

import scala.util.Try

object TryMatch extends App {
  class StatsException(mess: String) extends FileNotFoundException(mess)
  def main(i: Int): Try[Seq[String]] = Try[Seq[String]](i match {
    case 0 => Seq("1", "2", "3")
    case 1 => throw new StatsException("ololo")
    case 2 => throw new FileNotFoundException("alala")
    case 3 => throw new Exception("ilili")
  })

  def trying(i: Int) = main(i) map (_ foreach println) recover {
    case ex@(_: FileNotFoundException | _: StatsException) =>
      println(ex.getMessage)
    case ex =>
      ex.printStackTrace()
  }

  0 to 3 map trying
}
