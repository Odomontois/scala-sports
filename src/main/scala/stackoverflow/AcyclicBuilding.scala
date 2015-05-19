/**
 * Author: Oleg Nizhnik
 * Date  : 19.05.2015
 * Time  : 9:57
 */
package stackoverflow
import scala.collection.immutable.Queue

object AcyclicBuilding {
  case class Mapping(name: String, parents: Seq[String] = Nil)

  val mappings = Seq(
    Mapping("aaa", Seq("fff")),
    Mapping("bbb"),
    Mapping("ccc"),
    Mapping("ddd", Seq("aaa", "bbb")),
    Mapping("eee", Seq("ccc")),
    Mapping("fff", Seq("ddd")),
    Mapping("ggg", Seq("aaa", "fff")),
    Mapping("hhh")
  )

  class CyclicReferences(val nodes: Seq[String])
    extends RuntimeException(f"elements withing cycle detected: ${nodes mkString ","}")

  def buildTrees(data: Seq[Mapping]): Seq[Node] = {
    val parents = data.map(m => (m.name, m.parents)).toMap withDefaultValue Seq.empty
    val children = data.flatMap(m => m.parents map ((_, m.name))).groupBy(_._1).mapValues(_.map(_._2))

    def loop(queue: Queue[String], unresolved: Map[String, Set[String]], nodes: Map[String, Node]): TraversableOnce[Node] = queue match {
      case Seq() => if (unresolved.isEmpty) nodes.values else throw new CyclicReferences(unresolved.keys.toSeq)
      case key +: rest =>
        val (newQueue, newUnresolved) = ((rest, unresolved) /: parents(key)) { (pair, parent) =>
          val (queue, children) = pair
          val ch = children(parent) - key
          if (ch.isEmpty) (queue :+ parent, children - parent)
          else (queue, children.updated(parent, ch))
        }
        val node = Node(key, children.getOrElse(key, Seq.empty) map nodes)
        loop(newQueue, newUnresolved, nodes + (key -> node))
    }
    val initial = Queue(parents.keys.filter(key => !children.contains(key)).toSeq: _*)
    val unresolved = children mapValues (_.toSet) withDefaultValue Set.empty
    loop(initial, unresolved, Map()).filter(node => parents(node.name).isEmpty).toIndexedSeq
  }

  case class Node(name: String, children: Seq[Node] = Nil)

  val trees = buildTrees(mappings)

  def main(args: Array[String]) = trees.foreach(println)

}
