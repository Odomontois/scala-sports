/**
 * Author: Oleg Nizhnik
 * Date  : 20.05.2015
 * Time  : 15:47
 */
package daily
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.ListMap
import xml.XML
import java.net.URLEncoder

trait Places{
  def url:String
  val mems = xml.XML.load(url)
  val names = mems.descendant flatMap {
    case n if n.label == "sobjpan" && (n \ "@class").text == "mw-headline" => Seq(Left(n.text))
    case n if n.label == "table" => n \\ "td" \\ "span" \\ "b" map (n => Right(n.text))
    case _ => Seq()
  }
  val grouped = (names.foldLeft(("Default", Vector[String](), Vector[(String, Seq[String])]())) _ andThen {
    case (current, coll, acc) => acc :+ current -> coll
  }) { (state, elem) =>
    val (current, coll, acc) = state
    elem match {
      case Right(item) => (current, coll :+ item, acc)
      case Left(header) => (header, Vector(), acc :+ current -> coll)
    }
  }
  
  val gr = ListMap(grouped: _*)
  val json = grouped.map {
    case (header, items) => f""" "$header":[\n\t\t"${
      items map (_.replace("\"", "\\\"")) mkString "\",\n\t\t\""
    }"]"""
  } mkString("{\n\t", ",\n\t", "}")

  val postfix = ""

  //  Files.write(Paths.get("phout.json"), json.getBytes(UTF_8))
  def write(file: String, string: String) = Files.write(Paths.get(file), string.getBytes(UTF_8))
  def getPoint(name: String = "", url: Boolean = false) = {
    val theUrl = if (url) name
    else {
      val nmenc = name split ' ' map (URLEncoder.encode(_, "utf-8")) mkString "+"
      f"http://geocode-maps.yandex.ru/1.x/?geocode=$nmenc$postfix&results=1"
    }
    val xml = XML.load(theUrl)
    def parsePair(pair: String) = pair.split(' ') map (_.toDouble) match {
      case Array() => None
      case Array(x, y) => Some((x, y))
    }
    for {
      point <- (xml \\ "Point" \ "pos").headOption
      p <- parsePair(point.text)
    } yield p
  }

  val imHere = getPoint(
    "http://geocode-maps.yandex.ru/1.x/?geocode=%D0%9F%D1%80%D0%BE%D1%81%D0%BF%D0%B5%D0%BA%D1%82+%D0%92%D0%B5%D1%80%D0%BD%D0%B0%D0%B4%D1%81%D0%BA%D0%BE%D0%B3%D0%BE&results=1"
    , url = true).get

  type Point = (Double, Double)

  def dist(p1: Point, p2: Point = imHere): Double = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    math.sqrt(math.pow(x1 - x2, 2) + math.pow(y1 - y2, 2))
  }

  def distTo(name: String, url: Boolean = false) = for {
    point <- getPoint(name, url = url)
  } yield dist(point)

  def distStr(name: String) = distTo(name) getOrElse "???"


}

object Moscow   extends Places{
  def url = "https://ru.wikivoyage.org/wiki/%D0%9F%D1%80%D0%B8%D1%80%D0%BE%D0%B4%D0%BD%D1%8B%D0%B5_%D0%BF%D0%B0%D0%BC%D1%8F%D1%82%D0%BD%D0%B8%D0%BA%D0%B8_%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B8/%D0%9C%D0%BE%D1%81%D0%BA%D0%B2%D0%B0"
  override val postfix = ",%D0%9C%D0%BE%D1%81%D0%BA%D0%B2%D0%B0"
  def printObj(obj: String) =
    f"""
       |\tобъект: $obj
        |\tудалённость: ${distStr(obj)}
     """.stripMargin
  def objects = grouped(0)._2 sortBy (distTo(_)) map printObj mkString ("\n" + "-" * 30 + "\n")
}

object MoscowOblast extends Places{
  def url = "https://ru.wikivoyage.org/wiki/%D0%9F%D1%80%D0%B8%D1%80%D0%BE%D0%B4%D0%BD%D1%8B%D0%B5_%D0%BF%D0%B0%D0%BC%D1%8F%D1%82%D0%BD%D0%B8%D0%BA%D0%B8_%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B8/%D0%9C%D0%BE%D1%81%D0%BA%D0%BE%D0%B2%D1%81%D0%BA%D0%B0%D1%8F_%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D1%8C"
  override val postfix = ",%D0%9C%D0%BE%D1%81%D0%BA%D0%BE%D0%B2%D1%81%D0%BA%D0%B0%D1%8F+%D0%9E%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D1%8C"


  def printObj(obj: String) =
    f"""
       |\tобъект: $obj
        |\tудалённость: ${distStr(obj)}
     """.stripMargin

  def printPlace(name: String, objs: Seq[String]) =
    f"""
       |имя: $name
        |удалённость: ${distStr(name)}
        |количество: ${objs.size}
        |объекты: ${objs mkString("\n\t", "\n\t", "\n")}
     """.stripMargin

  def places = grouped sortBy { case (name, _) => distTo(name) } map (printPlace _).tupled mkString ("\n" + "-" * 30 + "\n")
}

