/**
 * Author: Odomontois
 * Date  : 25-Apr-15
 * Time  : 23:27
 */
package hackerrank.contest.lambdacalculi10


object FlipTheTree {
  object Tag extends Enumeration {
    val Begin, Node, End = Value
  }
  
  import Tag._
  
  case class TreeRange(pos: Int, from: Int, to: Int)
  
  def buildPosSeq(ps: IndexedSeq[Int]): IndexedSeq[TreeRange] = {
    val children = ps.zipWithIndex.sorted.foldLeft[List[(Int, Either[Int, (Int, Int)])]](Nil) { (acc, next) => acc match {
      case (p, Left(l)) :: rest if p == next._1 => (p, Right((l, next._2 + 2))) :: rest
      case _ => (next._1, Left(next._2 + 2)) :: acc
    }
    }.toMap
    def build(p: Int): Stream[(Int, Tag.Value)] = children.get(p).fold(values.map((p, _)).toStream) {
      case Left(l) => (p, Begin) #:: build(l) #:::(p, Node) #:: Stream((p, End))
      case Right((l, r)) => (p, Begin) #:: build(l) #:::(p, Node) #:: build(r) #::: Stream((p, End))
    }
    
    val dict = ((Map.empty[(Int, Tag.Value), Int], 1) /: build(1)) { (mappos, elem) =>
      val (map, pos) = mappos
      val (idx, tag) = elem
      tag match {
        case Begin => (map + (((idx, Begin), pos)), pos)
        case End => (map + (((idx, End), pos)), pos)
        case Node => (map + (((idx, Node), pos)), pos + 1)
      }
      
    }._1
    1 to (1 + ps.length) map (idx => TreeRange(dict((idx, Node)), dict((idx, Begin)), dict((idx, End))))
  }
  
  implicit class LinFuncOp(quot: (Int, Int)) {
    def ##(other: (Int, Int)) = {
      val (a, b) = quot
      val (c, d) = other
      (a * c, a * d + b)
    }
    
    def #*(x: Int) = {
      val (a, b) = quot
      a * x + b
    }

    def #@(tree: Tree) = tree match {
      case Branch(bop, l, r) => Branch(this ## bop, l, r)
      case Leaf(v) => Leaf(this #* v)
    }
  }
  val unit = (1, 0)
  
  sealed trait Tree
  
  case class Leaf(v: Int) extends Tree
  case class Branch(op: (Int, Int), left: Tree, right: Tree) extends Tree
  
  def buildTree(n: Int): (Int, Tree) = {
    def loop(stack: List[(Int, Tree)], e: Int, size: Int, sub: Tree): List[(Int, Tree)] = stack match {
      case (psize, ptree) :: others if psize == size => loop(others, e, size * 2, Branch(unit, ptree, sub))
      case _ if e < n => loop((size, sub) :: stack, e + 1, 1, Leaf(e + 1))
      case _ => (size, sub) :: stack
    }
    def finish(stack: List[(Int, Tree)]): (Int, Tree) = stack match {
      case List(single) => single
      case (sizer, tr) :: (sizel, tl) :: rest if sizer == sizel => finish((sizer * 2, Branch(unit, tl, tr)) :: rest)
      case (size, tree) :: rest => finish((size * 2, Branch(unit, tree, Leaf(0))) :: rest)
    }
    finish(loop(Nil, 1, 1, Leaf(1)))
  }
  
  def calc(tree: Tree, pos: Int, size: Int, op: (Int, Int) = unit): Int = tree match {
    case Leaf(v) => op #* v
    case Branch(bop, l, _) if pos <= (size / 2) => calc(l, pos, size / 2, op ## bop)
    case Branch(bop, _, r) => calc(r, pos - size / 2, size / 2, op ## bop)
  }

  import math.{max, min}
  
  def propagate(tree: Tree, from: Int, to: Int, size: Int, op: (Int, Int)): Tree =
    if (to <= 1 || from > size) tree //Out of bounds = no effect
    else if (from == 1 && to == size + 1) tree match {
      //full tree = change top
      case Branch(bop, l, r) => Branch(op ## bop, l, r)
      case Leaf(v) => Leaf(op #* v)
    } else tree match {
      //partial propagation  - propagate full operation on children first
      case Branch(bop, l, r) => Branch(unit,
        propagate(bop #@ l, from, min(size / 2 + 1, to), size / 2, op),
        propagate(bop #@ r, max(1, from - size / 2), to - size / 2, size / 2, op)
      )
      case Leaf(v) => sys.error("Could not apply partial propagation to leaf")
    }

  type TreeOp = Tree => Tree
  class QueryMatch(pos: IndexedSeq[TreeRange], size: Int) {

    def request(x: Int, y: Int): TreeOp = { tree =>
      val ix = calc(tree, pos(x - 1).pos, size)
      val iy = calc(tree, pos(y - 1).pos, size)
      println(if (ix < iy) "L" else "R")
      tree
    }

    def flip(x: Int): TreeOp = { tree =>
      val p = pos(x - 1)
      val from = calc(tree, p.from, size)
      val to = calc(tree, p.to - 1, size)
      val op = (-1, from + to)
      propagate(tree, p.from, p.to, size, op)
    }

    val debug: TreeOp = { tree => {
      println((for (i <- pos.indices) yield {
        val p = pos(i).pos
        val place = calc(tree, p, size)
        (place, i + 1)
      }).sorted.map { case (p, i) => f"$p: $i" } mkString " ,")
      tree
    }
    }

    object Request {
      def unapply(array: Array[Int]): Option[TreeOp] = array match {
        case Array(2, x, y) => Some(request(x, y))
        case _ => None
      }
    }
    object Flip {
      def unapply(array: Array[Int]): Option[TreeOp] = array match {
        case Array(1, x) => Some(flip(x))
        case _ => None
      }
    }
  }


  import io.StdIn._


  class Flipper(parents: IndexedSeq[Int]) {
    val n = parents.length + 1
    val (size, initial) = buildTree(n)
    val pos = buildPosSeq(parents)
    val queryMatch = new QueryMatch(pos, size)

    import queryMatch._


    def handleQueries(queries: Seq[Array[Int]]) =
      (initial /: queries) { (tree, query) =>
        query match {
          case Request(f) => f(tree)
          case Flip(f) => f(tree)
        }
      }
  }

  def main(args: Array[String]): Unit = {
    readLine()
    val parents = readLine() split " " map (_.toInt) toIndexedSeq
    val qn = readInt()

    val queries = 1 to qn map (_ => readLine split " " map (_.toInt))

    new Flipper(parents).handleQueries(queries)
  }
}
