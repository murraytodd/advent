package advent.Y2020

object Day6 {
  
  def uniques(lines: Seq[String]): Int = lines.map(_.toSet).reduce(_ union _).size
  def allYes(lines: Seq[String]): Int = lines.map(_.toSeq).reduce(_ intersect _).size

}
