/**
 * Author: Oleg Nizhnik
 * Date  : 02.06.2015
 * Time  : 10:41
 */
package stackoverflow
import scala.collection.immutable.BitSet
import scala.util.Random
import Numeric.Implicits._

object Stadium {
  def random(sectors: Int, rows: Int, seats: Int): Stadium = {
    val size = sectors * rows * seats
    new Stadium(sectors, rows, seats,
      BitSet fromBitMask Array.fill(size / 64 + 1)(Random.nextLong()) take size)
  }

  def apply(occupied: (Int, Int, Int)*): Stadium = {
    val sectors = occupied.map(_._1).max + 1
    val rows = occupied.map(_._2).max + 1
    val seats = occupied.map(_._3).max + 1
    val occNums = occupied map {
      case (sector, row, seat) => (sector * rows + row) * seats + seat
    }
    Stadium(sectors, rows, seats, BitSet(occNums: _*))
  }

  def income[N: Numeric](stadium: Stadium, standardPrice: N, premiumPrice: PartialFunction[Int, N] = PartialFunction.empty) = (
    for {
      sector <- 0 until stadium.sectors
      row <- 0 until stadium.rows
      price = premiumPrice.applyOrElse[Int, N](row, _ => standardPrice)
      occupied = implicitly[Numeric[N]].fromInt(stadium.occupiedInRow(sector, row))
    } yield price * occupied
    ).sum
}

case class Stadium(sectors: Int, rows: Int, seats: Int, occupied: BitSet) {
  @inline final def sectorSeats = rows * seats
  def rowRange(sector: Int, row: Int) = {
    val from = (sector * rows + row) * seats
    occupied.range(from, from + seats) map (_ - from)
  }
  def sectorRange(sector: Int) = {
    val from = sector * sectorSeats
    occupied.range(from, from + sectorSeats) map (_ - from)
  }
  lazy val wholeRow = BitSet(0 until seats: _*)
  def occupiedInRow(sector: Int, row: Int) = rowRange(sector, row).size
  def occupiedInSector(sector: Int) = sectorRange(sector).size
  def isOccupied(sector: Int, row: Int, seat: Int) = occupied(sector * sectorSeats + row * seats + seat)
  def isVacant(sector: Int, row: Int, seat: Int) = !isOccupied(sector, row, seat)
  def vacantInRow(sector: Int, row: Int) = wholeRow &~ rowRange(sector, row)
  def mostVacant(sector: Int) = 0 until rows maxBy (vacantInRow(sector, _).size)
  def occupiedSize = occupied.size
}
