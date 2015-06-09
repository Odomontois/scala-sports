/**
 * Author: Oleg Nizhnik
 * Date  : 02.06.2015
 * Time  : 9:50
 */
package stackoverflow
import scalaz._

object ScalaStateLens {
  case class Master(workers: Map[String, Worker])
  case class Worker(elapsed: Long, result: Vector[String])
  case class Message(workerId: String, work: String, elapsed: Long)

  type WorkerState[A] = State[Worker, A]
  def update(message: Message): WorkerState[Unit] = State.modify { w =>
    w.copy(elapsed = w.elapsed + message.elapsed,
      result = w.result :+ message.work)
  }
  def getWork: WorkerState[Vector[String]] = State.gets(_.result)
  def getElapsed: WorkerState[Long] = State.gets(_.elapsed)
  def updateAndGetElapsed(message: Message): WorkerState[Long] = for {
    _ <- update(message)
    elapsed <- getElapsed
  } yield elapsed

  type MasterState[A] = State[Master, A]
  def updateAndGetElapsedTime(message: Message): MasterState[Option[Long]] =
    State { m =>
      m.workers.get(message.workerId) match {
        case None => (m, None)
        case Some(w) =>
          val (newW, t) = updateAndGetElapsed(message).run(w)
          (m.copy(m.workers.updated(message.workerId, newW)), Some(t))
      }
    }
  def workersOf = Lens.lensu[Master, Map[String, Worker]]((m, w) => m.copy(w), _.workers)
  def workerByName(name: String) = workersOf.partial >=> PLens.mapVPLens(name)
  def updateAndGetElapsedTime2(message: Message): State[Master, Option[Long]] =
    workerByName(message.workerId) %%= updateAndGetElapsed(message)

}
