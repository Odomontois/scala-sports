import scalaz._
import Scalaz._

BigInt()

case class Point(x: Int, y: Int) {
  def right = copy(x = x + y)

  def left = copy(x = x - y)

  def up = copy(y = x + y)

  def down = copy(y = x - y)
}

val start = Point(3299, 7314)

def times(n:Int,f:Point=>Point) = Function.chain(Seq.fill(n)(f))

List[Point => Point] (
_.down
,_.up
,_.right
,_.right
,_.right
,_.right
,_.up
,_.right
,_.up
,_.right
,_.up
,_.up
,_.up
,_.up
,_.right
,_.up
,_.up
,_.right
,_.right
,_.up
,_.up
,_.left
,times(6014,_.right)
).scanLeft(start)((p, f) => f(p) ) foreach println


