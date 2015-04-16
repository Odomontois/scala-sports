/**
 * Author: Odomontois
 * Date  : 14-Apr-15
 * Time  : 23:36
 */
package gcodejam

import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._
import scala.io.Source
import scalaz.Scalaz._
import scalaz._


trait CodeJamApp[Result] {
  val input: Array[String] => String = args => f"codejam/${args(0)}.in"
  val output: Array[String] => String = args => f"codejam/${args(0)}.out"
  
  type Solution[A] = State[Stream[String], A]
  
  def getLine: Solution[String] = for (
    input <- get;
    _ <- put(input.tail)
  ) yield input.head

  val solution: Solution[Result]

  def main(args: Array[String]): Unit = {
    val out = output(args)
    val first #:: rest = Source fromFile input(args) getLines() toStream
    lazy val tests = first.toInt
    val results: Stream[String] = (1 to tests).toStream
      .map(_ => solution).sequence[Solution, Result].run(rest)._2
      .zip(Stream.from(1)).map { case (result, index) => f"Case #$index: $result" }

    Files write(Paths get output(args), results.asJava)
  }
}
