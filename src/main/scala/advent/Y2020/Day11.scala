package advent.Y2020

import scala.util.Try
import scala.annotation.tailrec

object Day11 {

  enum Space(val txt: Char) {
    case EmptySeat extends Space('L')
    case FullSeat extends Space('#')
    case Floor extends Space('.')
  }

  case class Seating(spaces: IndexedSeq[IndexedSeq[Space]]) {
    override def toString: String = "\n" + spaces.map(_.map(_.txt).mkString).mkString("\n") + "\n"
    val rows: Int = spaces.size
    val cols: Int = spaces.headOption.getOrElse(IndexedSeq.empty).size
    def get(row: Int, col: Int): Option[Space] = {
      Try(spaces(row)(col)).toOption
    }
    def occupancy: Int = spaces.flatten.count(_ == Space.FullSeat)
    def neighbors(row: Int, col: Int): Int = {
      val n = for {
        r <- (row - 1) to (row + 1)
        c <- (col - 1) to (col + 1)
        if ! ((r == row) && (c == col)) // don't count self
        s <- get(r,c)
      } yield (s)
      n.count(_ == Space.FullSeat)
    }
    def lineOfSightNeighbors(row: Int, col: Int): Int = Seating.directions
      .map(d => lineOfSight(row, col, d._1, d._2))
      .count(_ == Space.FullSeat)
    def debugNeighbors: String = {
      (0 to rows).map { r =>
        (0 to cols).map { c =>
          neighbors(r,c).toString
        }.mkString
      }.mkString("\n")
    }
    def debugLineOfSightNeighbors: String = {
      (0 to rows).map { r =>
        (0 to cols).map { c =>
          lineOfSightNeighbors(r,c).toString
        }.mkString
      }.mkString("\n")
    }
    def lineOfSight(row: Int, col: Int, rowDirection: Int, colDirection: Int): Space = {
      LazyList.from(1).map(i => get(rowDirection * i + row, colDirection * i + col)).takeWhile(_.isDefined)
        .flatten
        .find(_ != Space.Floor).getOrElse(Space.Floor)
    }
    def newState(row: Int, col: Int): Space = {
      (get(row,col), neighbors(row,col)) match {
        case (Some(Space.EmptySeat), x) if x == 0 => Space.FullSeat
        case (Some(Space.FullSeat), x) if x >= 4 => Space.EmptySeat
        case (Some(s), _) => s
        case (None,_) => throw Exception(s"($row, $col) is not a valid space.")
      }
    }
    def lineOfSightNewState(row: Int, col: Int): Space = {
      (get(row,col), lineOfSightNeighbors(row,col)) match {
        case (Some(Space.EmptySeat), x) if x == 0 => Space.FullSeat
        case (Some(Space.FullSeat), x) if x >= 5 => Space.EmptySeat
        case (Some(s), _) => s
        case (None,_) => throw Exception(s"$row, $col) is not a valid space.")
      }
    }
    def updateState: Seating = {
      val seats = IndexedSeq.range(0, rows).map { row =>
        IndexedSeq.range(0, cols).map { col =>
          newState(row,col)
        }
      }
      Seating(seats)
    }
    def updateLineOfSightState: Seating = {
      val seats = IndexedSeq.range(0, rows).map { row =>
        IndexedSeq.range(0, cols).map { col =>
          lineOfSightNewState(row,col)
        }
      }
      Seating(seats)
    }
    def lineOfSightToConvergence: Seating = {
      val updated = this.updateLineOfSightState
      if (updated == this) this else updated.lineOfSightToConvergence
    }
    @tailrec
    final def toConvergence: Seating = {
      val updated = this.updateState
      if (updated == this) this else updated.toConvergence
    }
  }

  object Seating {
    val directions = for { row <- -1 to 1; col <- -1 to 1; if (! ((row == 0) && (col == 0))) } yield (row,col)
    def fromString(data: Array[String]) = {
      Seating(
        data.map(_.map { _ match 
          {
            case 'L' => Space.EmptySeat
            case '#' => Space.FullSeat
            case _ => Space.Floor
          }
        }.toIndexedSeq).toIndexedSeq
      )
    }
  }

}
