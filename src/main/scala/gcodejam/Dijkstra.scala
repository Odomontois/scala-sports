/**
 * Author: Odomontois
 * Date  : 11-Apr-15
 * Time  : 22:50
 */
package gcodejam

import utils.CodeJamApp

import scala.language.implicitConversions

object Dijkstra extends CodeJamApp {
//  implicit class QuatDigit(char: Char) {
//    def toQuat = char match {
//      case 'i' => I
//      case 'j' => J
//      case 'k' => K
//    }
//  }
//  implicit def oneToQuat(num: Int): Quat = num match {
//    case 1 => One
//    case -1 => -One
//    case _ => sys.error("use 1 or -1")
//  }
//  sealed trait Quat {
//    def *(e: Quat): Quat = this match {
//      case Minus(x) => Minus(x * e)
//      case One => e
//      case x: Imag => e match {
//        case Minus(u) => Minus(x * u)
//        case One => x
//        case y: Imag => x mul y
//      }
//    }
//
//    def unary_- = Minus(this)
//  }
//  sealed trait Imag extends Quat {
//    def mul(elem: Imag): Quat
//  }
//  case object One extends Quat
//  case object I extends Imag {
//    override def mul(elem: Imag): Quat = elem match {
//      case I => -1
//      case J => K
//      case K => -J
//    }
//  }
//  case object J extends Imag {
//    override def mul(elem: Imag): Quat = elem match {
//      case I => -K
//      case J => -1
//      case K => I
//    }
//  }
//  case object K extends Imag {
//    override def mul(elem: Imag): Quat = elem match {
//      case I => J
//      case J => -I
//      case K => -1
//    }
//  }
//  case class Minus(elem: Quat) extends Quat
//  case object Minus {
//    def apply(elem: Quat) = elem match {
//      case Minus(e) => e
//      case _ => new Minus(elem)
//    }
//  }
//  def minimalPrefixToGain(quats: Seq[Quat], aim: Quat, start: Quat, acc: Int): Option[Int] =
//    if (start == aim) Some(acc)
//    else if (quats.isEmpty) None
//    else minimalPrefixToGain(quats.tail, aim, start * quats.head, acc + 1)
//
//  def minimalPrefixRepeat(quats:Seq[Quat], aim:Quat):Option[Int] = {
//    lazy val whole = quats reduce (_ * _)
//    Stream.iterate[Quat](One)(whole * _) take 8 map
//      (minimalPrefixToGain(quats, aim, _, 0)) zip
//      Stream.from(0,quats.length) map
//      (())
//  }
//
  override def solution(source: Stream[String]): (String, Stream[String]) = {
    ("", source)
  }
}
