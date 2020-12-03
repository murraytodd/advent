package advent.Y2020

object Day1 {

  def findPairMulti(target: Int, data: Seq[Int]): Long = {
    val answer = for { // get all (x,y) pairs
      x <- data
      y <- data if (x < y) && (x + y == target) // remove (x,y) (y,x) duplicates & add restriction
    } yield (x * y)
    assert(answer.size == 1, "There should be one unique result.")
    answer.head
  }

  def findTrioMulti(target: Int, data: Seq[Int]): Long = {
    val answer = for {
      x <- data
      y <- data if (x < y)
      z <- data if (y < z) && (x + y + z == target)
    } yield (x.toLong * y * z)
    assert(answer.size == 1, "There should be one unique result")
    answer.head
  }
}
