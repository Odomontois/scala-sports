package utils

import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._
import scala.io.Source

/**
 * Author: Odomontois
 * Date  : 11-Apr-15
 * Time  : 16:48
 */

trait CodeJamApp {
  def solution(source: Stream[String]): (String, Stream[String])

  def main(args: Array[String]): Unit = {
    def input: String = f"codejam/${args(0)}.in"
    def output: String = f"codejam/${args(0)}.out"

    lazy val source = Source fromFile input getLines() toStream

    lazy val tests = source.head.toInt

    lazy val result = ((Stream.empty[String], source.tail) /: (1 to tests))(
      (iter, test: Int) => {
        val (strings, source) = iter
        val (result, used) = solution(source)
        (strings ++ Stream(f"Case #$test: $result"), used)
      })._1

    Files write(Paths get output, result.asJava)
  }
}
