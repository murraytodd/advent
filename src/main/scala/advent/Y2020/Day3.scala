package advent.Y2020

object Day3:

  class TreeMap(data: Seq[String]):
    val trees = data.map(_.toCharArray.map { case '.' => false; case '#' => true }.toIndexedSeq)
    val rows = trees.size
    def isTree(right: Int, down: Int) = if (down >= rows) false else trees(down)(right % trees(down).size)
    def countSlope(right: Int, down: Int): Int = (1 until rows).count(r => isTree(r * right, r * down))
