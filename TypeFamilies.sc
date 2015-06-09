import scala.annotation.tailrec

trait ReducerProvider {
  type K
  type V
}
case class KeyVal[Key, Val](key: Key, value: Val) extends ReducerProvider {
  type K = Key
  type V = Val
}
@tailrec def x(int: Int): Int = x(int)



type MapOutput = KeyVal[String, Int]
object ReducerTestMain extends App {
  def mapFun(s: String): MapOutput = KeyVal(s, 1)

  val red = new ReducerComponent[MapOutput]((a: Int, b: Int) => a + b)
  val data = List[String]("a", "b", "c", "b", "c", "b")
  data foreach { s => red(mapFun(s)) }
  println(red.mem)
  // OUTPUT: Map(a -> 1, b -> 3, c -> 2)
}
class ReducerComponent[T <: ReducerProvider](f: (T#V, T#V) => T#V) {
  var mem = Map[T#K, T#V]()

  def apply(kv: KeyVal[T#K, T#V]) = {
    val KeyVal(k, v) = kv
    mem += (k -> (if (mem contains k) f(mem(k), v) else v))
  }
}
val red = new ReducerComponent[MapOutput](_ + _)

red(KeyVal("a", 2))
red(KeyVal("b", 3))
red(KeyVal("a", 5))
red.mem
