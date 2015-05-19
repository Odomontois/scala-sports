import scala.util.{Failure, Success, Try}
implicit class LogTry[T](attempt: Try[T]) {
  def log[E](comp: T => E, block: String => String, error: Throwable => String = _ => "(not available)") =
    println(block(attempt map (comp andThen (_.toString)) recover { case ex => error(ex) } get))
}
val size: Try[(Int, Int)] = Success((1, 2))
size.log(_._2, size => f"size is $size kb")
Failure[(Int, Int)](new RuntimeException).log(_._2, size => f"size is $size kb", err => f"(not available because $err)")