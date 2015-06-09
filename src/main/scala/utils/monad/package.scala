/**
 * Author: Odomontois
 * Date  : 25-Apr-15
 * Time  : 19:48
 */
package utils

package object monad {
  case class State[S, A](run: S => (S, A)) {
    def map[B](f: A => B) = State { (s: S) =>
      val (s1, y) = run(s)
      (s1, f(y))
    }

    def flatMap[B](f: A => State[S, B]) = State { (s: S) =>
      val (s1, y) = run(s)
      f(y).run(s1)
    }
  }
  def put[S](x: S) = State((s: S) => (x, x))

  def get[S] = State((s: S) => (s, s))

  def state[S, A](x: A) = State { (s: S) => (s, x) }

  def foldState[S, A, B](z: State[S, B], seq: Seq[A])(f: (B, A) => State[S, B]): State[S, B] = seq match {
    case Seq() => z
    case a +: as => foldState(for {
      b <- z
      b1 <- f(b, a)
    } yield b1, as)(f)
  }
}
