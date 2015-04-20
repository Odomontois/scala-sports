/**
 * Author: Oleg Nizhnik
 * Date  : 20.04.2015
 * Time  : 16:36
 */
package hackerrank.algorithm.number

object SpecialMultiple extends App {
  type Nums = Stream[(List[Char], Int)]

  def solution(x: Int) = {
    def produce(nums: Nums) = for {
      (num, rem) <- nums
      dig <- Seq(0, 9)
    } yield (('0' + dig).toChar :: num, (rem * 10 + dig) % x)

    def find(nums: Nums): String = nums find (_._2 == 0) match {
      case Some((num, _)) => num.reverse.mkString
      case None => find(produce(nums))
    }

    val initial: Nums = Stream((List('9'), 9 % x))

    find(initial)
  }

  1 to io.StdIn.readInt map (_ => io.StdIn.readInt) foreach (solution _ andThen println)
}
