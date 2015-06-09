/**
 * Author: Odomontois
 * Date  : 25-Apr-15
 * Time  : 18:15
 */
package hackerrank.contest.lambdacalculi10

object KnightAndQueen extends App {
  def grundy(moves: TraversableOnce[Int]): Int = {
    val ms = moves.toSet
    Iterator.from(0).find(!ms(_)).get
  }

  val movesSeq = for {
    i <- 0 to 50
    j <- 0 to 50
  } yield (i, j)

  def fillMoves(f: ((Int, Int)) => Seq[(Int, Int)]) = (Map.empty[(Int, Int), Int] /: movesSeq) { (game, move) =>
    game.updated(move, grundy(f(move) map game))
  }

  val queen = fillMoves {
    case (x, y) =>
      (0 to (x - 1) map (x1 => (x1, y))) ++
        (0 to (y - 1) map (y1 => (x, y1))) ++
        (1 to math.min(x, y) map (t => (x - t, y - t)))
  }
  val knight = fillMoves {
    case (_, 0) | (0, _) | (1, 1) => Seq()
    case (x, 1) => Seq((x - 2, 0))
    case (1, y) => Seq((0, y - 2))
    case (x, y) => Seq((x - 1, y - 2), (x - 2, y - 1))
  }
  1 to io.StdIn.readInt foreach { _t =>
    val Array(xk, yk, xq, yq) = io.StdIn.readLine split " " map (_.toInt)
    println(if ((knight((xk, yk)) ^ queen((xq, yq))) != 0) "WIN" else "LOSE")
  }
}
