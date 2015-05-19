/**
 * Author: Oleg Nizhnik
 * Date  : 18.05.2015
 * Time  : 13:34
 */
package stackoverflow

import scala.collection.mutable.ArrayBuffer

object Adjacency {
  class Graph[T](val vertex: IndexedSeq[T], val edges: Seq[(Int, Int)]) {
    def size: Int = vertex.length
    lazy val index: Map[T, Int] = vertex.zipWithIndex.toMap
    lazy val adjacent = {
      val dict = edges groupBy (_._1) mapValues (_ map (_._2)) withDefaultValue Seq.empty
      0 until size map dict
    }
    def addVertices(vs: T*) = new Graph(vertex ++ vs, edges)
    def addEdges(es: (Int, Int)*) = new Graph(vertex, edges ++ es)
    def addNamedEdges(es: (T, T)*) = addEdges(es map { case (ix1, ix2) => index(ix1) -> index(ix2) }: _*)
    def adjacencyMatrix = adjacent map (_.toSet) map (0 until size map _)
    def printEdges: String = {
      for(idx <- 0 until size)
        yield f"vertex $idx: ${adjacent(idx) mkString " "}"
    } mkString "\n"
    def printAdjacencyList: String =
      adjacent.zipWithIndex map { case (vals, idx) => vals map vertex mkString(f"${vertex(idx)}: ", ", ", "") } mkString "\n"
    def printAdjacencyMatrix: String = {0 until size map (idx => f"$idx%3d") mkString("index|", "|", "|\n")} +
      "-" * (size * 4 + 6) + "\n" +
      (adjacencyMatrix.zipWithIndex map { case (vals, idx) => vals.map(if (_) 'X' else '.').mkString(f"$idx%5d| ", " | ", " |\n") } mkString)
  }

  def main(args: Array[String]) {
    def vertices = Vector("Seattle", "San Francisco", "Los Angeles",
      "Denver", "Kansas City", "Chicago", "Boston", "New York",
      "Atlanta", "Miami", "Dallas", "Houston")

    def edges = Vector(
      (0, 1), (0, 3), (0, 5),
      (1, 0), (1, 2), (1, 3),
      (2, 1), (2, 3), (2, 4), (2, 10),
      (3, 0), (3, 1), (3, 2), (3, 4), (3, 5),
      (4, 2), (4, 3), (4, 5), (4, 7), (4, 8), (4, 10),
      (5, 0), (5, 3), (5, 4), (5, 6), (5, 7),
      (6, 5), (6, 7),
      (7, 4), (7, 5), (7, 6), (7, 8),
      (8, 4), (8, 7), (8, 9), (8, 10), (8, 11),
      (9, 8), (9, 11),
      (10, 2), (10, 4), (10, 8), (10, 11),
      (11, 8), (11, 9), (11, 10)
    )
    val graph = new Graph(vertices, edges)
      .addVertices("Moscow", "Volgograd")
      .addNamedEdges(
        "Moscow" -> "Volgograd",
        "Moscow" -> "New York",
        "New York" -> "Moscow",
        "Volgograd" -> "Dallas")

    println("number of vertices in graph: " + graph.size)
    println("the vertex with index 1 is: " + graph.vertex(1))
    println("the index for Miami is: " + graph.index("Miami"))
    println("the edges for graph: ")
    println(graph.printEdges)
    println("adjacency list for graph: ")
    println(graph.printAdjacencyList)
    println("adjacency matrix for graph: ")
    println(graph.printAdjacencyMatrix)
  }
}
