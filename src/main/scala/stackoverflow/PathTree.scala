/**
 * Author: Oleg Nizhnik
 * Date  : 08.05.2015
 * Time  : 10:44
 */
package stackoverflow

object PathTree extends App {
  import scalaz._
  import Scalaz._
  val paths = List(List("foo"), List("bar", "a"), List("bar", "b", "c"))

  def pathTree[E](root: E, paths: Seq[Seq[E]]): Tree[E] =
    root.node(paths groupBy (_.head) map {
      case (parent, subpaths) => pathTree(parent, subpaths collect {
        case parent +: rest if rest.nonEmpty => rest
      })
    } toSeq: _*)

  def addPath[E](path: Seq[E], tree: Tree[E]): Tree[E] = if (path.isEmpty) tree
  else
    tree match {
      case Tree.Node(root, children)
        if children.exists(_.rootLabel == path.head) => root.node(
        children map (subtree => if (subtree.rootLabel == path.head) addPath(path.tail, subtree) else subtree): _*
      )
      case Tree.Node(root, children) => root.node(children :+ path.init.foldRight(path.last.leaf)((root, sub) => root.node(sub)): _*)
    }

  val tree = pathTree("root", List(List("foo"), List("bar", "a"), List("bar", "b", "c")))
  println(tree.drawTree)
  println(addPath(Seq("foo","baz"), tree).drawTree)
}
