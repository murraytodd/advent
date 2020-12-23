package advent.Y2020

import scala.annotation.tailrec

object Day10 {
  
  def adapterSteps(joltages: List[Int]): Map[Int,Int] =
    (3 :: (0 :: joltages).sorted.sliding(2).map(l => l(1) - l(0)).toList)
      .groupBy(identity).view.mapValues(_.size).toMap

  implicit class IntPow[N: Numeric](x: N) {
    def **(e: Int)(implicit nt: Numeric[N]): N = Seq.fill(e)(x).foldLeft(nt.one)(nt.times(_,_))
  }

  def adapterPermutations(joltages: List[Int]): Long = {
    val devicePower = joltages.max + 3
    // I'm improperly adding a 3 step in the front, for pattern matching
    val steps = (3 :: (devicePower :: 0 :: joltages).sorted.sliding(2).map(l => l(1) - l(0)).toList)
    val fourOnes = steps.sliding(6).count(_.toList == List(3,1,1,1,1,3))
    val threeOnes = steps.sliding(5).count(_.toList == List(3,1,1,1,3))
    val twoOnes = steps.sliding(4).count(_.toList == List(3,1,1,3))
    (7L ** fourOnes) * (4L ** threeOnes) * (2L ** twoOnes)
  }

  def expand(jumps: Seq[Int]): Seq[Int] = (0 until jumps.size).map(0 until jumps.size).map(i => (0 to i).map(jumps).sum)

  def isValid(joltages: Seq[Int]): Boolean = joltages.sliding(2).forall(i => i(1) - i(0) <= 3)

  def getDropPermutations[T](segment: Seq[T]): Seq[Seq[T]] = {
    if (segment.isEmpty || segment.tail.isEmpty) {
      Seq[Seq[T]](segment)
    } else {
      val children = (0 until segment.size)
        .map(i => segment.slice(0,i) ++ segment.slice(i+1,segment.size))
      (children ++ children.flatMap(getDropPermutations)).distinct
    }
  }

  def countValidPermutations(segment: Seq[Int]): Long = {
    if (segment.size < 3) {
      1
    } else {
      val first = segment.head
      val last = segment.last
      val body = segment.drop(1).dropRight(1)
      val perms = (Seq.empty[Int] :: body :: getDropPermutations(body).toList).distinct
      perms.map(b => first +: b :+ last).count(isValid)
    }
  }

  @tailrec
  def chunkSegments(remainder: Seq[Int], chunks: List[Seq[Int]] = List.empty): List[Seq[Int]] = {
    val dropQty = remainder.sliding(2).indexWhere(i => i(1) - i(0) < 3)
    val trimmed = remainder.drop(dropQty)
    if (trimmed.size < 2) {
      chunks.reverse
    } else {
      val newChunkQty = trimmed.sliding(2).indexWhere(i => i(1) - i(0) == 3) + 1
      if (newChunkQty == 0) {
        (trimmed :: chunks).reverse
      } else {
        val newRemainder = trimmed.drop(newChunkQty)
        if (newRemainder.size < 2) {
          (trimmed.drop(newChunkQty) :: chunks).reverse
        } else {
           chunkSegments(trimmed.drop(newChunkQty), trimmed.take(newChunkQty) :: chunks)
        }
      }
    }
  }

  def countTotal(joltages: Seq[Int]): Long = chunkSegments(0 +: joltages.sorted)
    .map(countValidPermutations).foldLeft(1L)(_ * _)

}
