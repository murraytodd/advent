package advent.Y2020

import scala.util.Using

object Day2 {

  case class Entry(min: Int, max: Int, element: Char, pw: String) {
    def isValid: Boolean = {
      val occurances = pw.count(_ == element)
      (occurances >= min) && (occurances <= max)
    }
    def isValid2: Boolean = {
      val countTrue: Int = ( if (pw(min - 1) == element) 1 else 0 ) + ( if (pw(max - 1) == element) 1 else 0 )
      countTrue == 1
    }
  }

  object Entry {
    val format = "([0-9]+)-([0-9]+) (.): (.*)".r
    def apply(line: String): Entry = line match { case format(min,max,let,pw) => Entry(min.toInt, max.toInt, let.head, pw) }
  }

  val data = Using(scala.io.Source.fromFile("2020-day2-input.txt")) { _.getLines().map(Entry.apply).toList }.get

  val answer = data.count(_.isValid)
  val answer2 = data.count(_.isValid2)
}
