import scala.util.{Failure, Success, Try}
/**
 * Author: Oleg Nizhnik
 * Date  : 08.07.2015
 * Time  : 10:40
 */
object run {
  implicit class FoldableTry[T](tryable: Try[T]) {
    def fold[X](failure: Throwable => X)(success: T => X): X = {
      tryable match {
        case Success(result) => success(result)
        case Failure(ex) => failure(ex)
      }
    }
  }

  def main(args: Array[String]) {
    Success(2).fold(_ => 0)(_ => 1)
  }
}
