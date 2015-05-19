/**
 * Author: Oleg Nizhnik
 * Date  : 19.05.2015
 * Time  : 15:23
 */
package daily
import akka.actor._
import akka.event.LoggingReceive
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import Numeric.Implicits._
import concurrent.duration._
import concurrent.ExecutionContext.Implicits._

import reflect.runtime.universe._
import scala.concurrent.Await

class SummingActor[N: Numeric : TypeTag] extends AbstractLoggingActor {
  import SummingActor._
  val num = implicitly[Numeric[N]]
  import num._
  val tpe = typeOf[N]
  override def receive = summing(zero)

  def summing(sum: N): Receive = {
    case a@Add(n: N) if a.tag.tpe <:< tpe => context become summing(sum + n)
    case a: Add[N] => log.error(f"$a is of wrong type ${a.tag.tpe.toString}")
    case Request => sender ! Response(sum)
  }
}

class SummingMain extends AbstractLoggingActor {
  import SummingActor._


  override def receive = LoggingReceive {
    case Activate(n) =>
      val sum = context.actorOf(Props[SummingActor[Long]](new SummingActor[Long]), "summator")
      1 to n foreach (i => sum ! Add(i.toLong))
      sum ! Request
      sum ! PoisonPill
      context become active(sender)
  }
  def active(papa: ActorRef) = LoggingReceive {
    case Response(sum: Long) =>
      log.info(f"long summ is $sum")
      papa ! sum
      context stop self
  }
}

object SummingActor {
  case class Add[N: Numeric : TypeTag](a: N)(implicit val tag: TypeTag[N])
  case object Request
  case class Response[N: Numeric : TypeTag](a: N)(implicit val tag: TypeTag[N])
  case class Activate(n: Int)

  def main(args: Array[String]) = {
    val system = ActorSystem("summing")
    val main = system actorOf Props[SummingMain]
    implicit val timeout = Timeout.durationToTimeout(5 seconds)
    val sum = Await.result((main ? Activate(1000000)).mapTo[Long], Duration.Inf)
    println(f"summ is $sum")
    system.awaitTermination(10 seconds)
  }
}
