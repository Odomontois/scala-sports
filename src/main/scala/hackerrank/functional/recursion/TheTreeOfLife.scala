/**
 * Author: Oleg Nizhnik
 * Date  : 01.06.2015
 * Time  : 16:09
 */
package hackerrank.functional.recursion
import scala.annotation.tailrec

object TheTreeOfLife {
  trait Tree {
    def cell: Byte
    def step(r: Rule, parent: Byte): Tree
    def left: Tree
    def right: Tree
  }
  class Leaf(val cell: Byte) extends Tree {
    def step(rule: Rule, parent: Byte) = new Leaf(rule.next(parent, 0, cell, 0))
    def left = new Leaf(0)
    def right = new Leaf(0)
  }
  class Branch(val cell: Byte, _left: => Tree, _right: => Tree) extends Tree {
    lazy val left = _left
    lazy val right = _right
    def step(rule: Rule, parent: Byte) = new Branch(
      rule.next(parent, left.cell, cell, right.cell),
      left.step(rule, cell),
      right.step(rule, cell))
  }
  trait ParseState {
    def next(tree: Tree): ParseState
  }
  case class Done(tree: Tree) extends ParseState {
    def next(tree: Tree) = ErrorState("tree is done")
  }
  case object Init extends ParseState {
    def next(tree: Tree) = Done(tree)
  }
  case class PLeft(up: ParseState) extends ParseState {
    def next(left: Tree) = PCenter(up, left)
  }
  case class PCenter(up: ParseState, left: Tree) extends ParseState {
    def next(center: Tree) = PRight(up, left, center.cell)
  }
  case class PRight(up: ParseState, left: Tree, cell: Byte) extends ParseState {
    def next(right: Tree) = Waiting(up, new Branch(cell, left, right))
  }
  case class Waiting(up: ParseState, tree: Tree) extends ParseState {
    def next(_t: Tree) = ErrorState("expecting )")
  }
  case class ErrorState(message: String) extends ParseState {
    def next(_t: Tree) = this
  }
  trait Rule {
    def next(parent: Byte, left: Byte, self: Byte, right: Byte): Byte
  }
  object Rule {
    def apply(code: Int) = new Rule {
      def next(parent: Byte = 0, left: Byte = 0, self: Byte, right: Byte = 0) = {
        val idx = parent << 3 | left << 2 | self << 1 | right
        ((code & 1 << idx) >> idx).toByte
      }
    }
  }
  case class CAState(rule: Rule, tree: Tree, prev: Option[CAState] = None) {
    lazy val next = CAState(rule, tree.step(rule, 0), Some(this))
    @tailrec final def evolve(steps: Int): CAState =
      if (steps == 0) this
      else if (steps > 0) next.evolve(steps - 1)
      else prev match {
        case Some(state) => state.evolve(steps + 1)
        case None => throw new RuntimeException("step back from beginning")
      }
  }

  def eval(tree: Tree, path: Seq[Either[Unit, Unit]]): Char = path match {
    case Seq() => if (tree.cell == 0) '.' else 'X'
    case Left(()) +: rest => eval(tree.left, rest)
    case Right(()) +: rest => eval(tree.right, rest)
  }

  def parse(code: String) = code.foldLeft[ParseState](Init) { (state, char) => char match {
    case '(' => PLeft(state)
    case ')' => state match {
      case Waiting(up, tree) => up.next(tree)
      case _ => ErrorState("unexpected )")
    }
    case 'X' => state.next(new Leaf(1))
    case '.' => state.next(new Leaf(0))
    case ' ' => state //ignoring spaces
    case c => ErrorState(f"unexpected symbol: $c")
  }
  } match {
    case Done(tree) => tree
    case _ => throw new RuntimeException(f"incomplete tree")
  }
  val request = "(-?\\d+)\\s+\\[([<>]*)\\]".r

  def main(args: Array[String]) {
    import io.StdIn._
    val rule = Rule(readInt())
    val tree = parse(readLine())
    (CAState(rule, tree) /: (1 to readInt())) { (state, _) => readLine match {
      case request(stepsStr, pathStr) =>
        val steps = stepsStr.toInt
        val path = pathStr map {
          case '<' => Left(())
          case '>' => Right(())
        }
        val newState = state.evolve(steps)
        println(eval(newState.tree, path))
        newState
    }
    }
  }
}
