/**
 * Author: Odomontois
 * Date  : 13-May-15
 * Time  : 00:30
 */
package stackoverflow



object StringMatcher extends App {
  val ZeroFillDec = "%0(\\d+)d(.*)".r
  val SimpleDec = "%(\\d+)d(.*)".r
  val String = "%s(.*)".r


  implicit class StringMatcher(val sc: StringContext){
    object pat {
      def unapplySeq(s: String): Option[Seq[String]] = {
        val re = (sc.parts.head ++ (sc.parts.tail map patternize)).mkString.r
        re.unapplySeq(s)
      }
    }
  }

  def patternize(part: String) = part match {
    case ZeroFillDec(len, rest) => f"(\\d{${len.toInt}})$rest"
    case SimpleDec(len, rest) => f"(\\d{1,${len.toInt}})$rest"
    case String(rest) => f"(.*)$rest"
    case rest => f"(.*)$rest"
  }

}

