package advent.Y2020

object Day5 {

  case class Seat(code: String) {
    val row = Integer.parseInt(code.take(7).replaceAll("F","0").replaceAll("B","1"), 2)
    val col = Integer.parseInt(code.drop(7).replaceAll("L","0").replaceAll("R","1"), 2)
    val id = row * 8 + col
  }

}
