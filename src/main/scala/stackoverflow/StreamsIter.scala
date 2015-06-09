/**
 * Author: Odomontois
 * Date  : 16-May-15
 * Time  : 16:07
 */
package stackoverflow

object StreamsIter {
  class PStateM[Elem, Result](val receiveM: Option[Elem] => (Option[PStateM[Elem, Result]], Option[Result]))
  class PState[Elem, Result](val receive: Elem => (PStateM[Elem, Result], Option[Result])) extends PStateM[Elem, Result]({
    case Some(elem) => receive(elem) match {case (next, res) => (Some(next), res)}
    case None => (None, None)
  })

  def process[Elem, Result](state: PStateM[Elem, Result])(source: Seq[Elem]): Stream[Result] = source match {
    case Seq() => state.receiveM(None)._2.toStream
    case head +: tail => {
      val (next, res) = state.receiveM(Some(head))
      next match {
        case None => res.toStream
        case Some(nState) => res match {
          case None => process(nState)(tail)
          case Some(res) => res #:: process(nState)(tail)
        }
      }
    }
  }

  def processIt[Elem, Result](state: PStateM[Elem, Result])(source: Iterator[Elem]): Iterator[Result] =
    if (!source.hasNext) state.receiveM(None)._2.toIterator
    else {
      val head = source.next()
      val (next, res) = state.receiveM(Some(head))
      next match {
        case None => res.toIterator
        case Some(nState) => res match {
          case None => processIt(nState)(source)
          case Some(res) => Iterator(res) ++ processIt(nState)(source)
        }
      }
    }


  case class MapS[A, B](f: A => B) extends PState[A, B](elem => (MapS(f), Some(f(elem))))
  case class FoldS[A, B](z: B, f: (B, A) => B) extends PStateM[A, B]({
    case Some(elem) => (Some(FoldS(f(z, elem), f)), None)
    case None => (None, Some(z))
  })
  case class FilterS[A](f: A => Boolean) extends PState[A, A](elem => (FilterS(f), Some(elem) filter f))



  def main(args: Array[String]): Unit = {
    val oddSum = process(FilterS[Int](_ % 2 == 1)) _ andThen process(FoldS[Int, Long](0L, _ + _.toLong)) andThen (_.head)
    println (oddSum(1 to 10000000))
  }

  trait Node
  case class Branch(elem: Int, children: IndexedSeq[Int]) extends Node
  case class Leaf(elem: Int) extends Node
  //  case class NodeElem() extends PState[Int, Node]( ??? )
  //  case class NodeChildCnt(elem:Int)  extends PState[Int,Node] ( ???)
  //  case class NodeChildElem(elem:Int, childCnt:Int, children:Stream[Int]) extends PState[Int,Node] (???)
  case object NodeElem extends PState[Int, Node](elem => (NodeChildCnt(elem), None))

  case class NodeChildCnt(elem: Int) extends PState[Int, Node]({
    case 0 => (NodeElem, Some(Leaf(elem)))
    case count => (NodeChildElem(elem, count, IndexedSeq.empty), None)
  })

  case class NodeChildElem(elem: Int, count: Int, children: IndexedSeq[Int]) extends PStateM[Int, Node]({
    case Some(child) =>
      if (count > 1)
        (Some(NodeChildElem(elem, count - 1, children :+ child)), None)
      else (Some(NodeElem), Some(Branch(elem, children :+ child)))
    case None => (None, Some(Branch(elem, children)))

  })
}
