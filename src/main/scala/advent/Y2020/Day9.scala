package advent.Y2020

import scala.annotation.tailrec

object Day9:

  def validate(range: Seq[Long], target: Long): Boolean = 
    {
      for
        x <- range
        y <- range if x < y
        if (x + y == target)
      yield (x + y)
    }.nonEmpty
  
  def firstInvalid(range: Seq[Long], window: Int): Option[Long] =
    range.sliding(window + 1).find(w => ! validate(w.dropRight(1),w.last)).map(_.last)

  def findSliceSum(data: Seq[Long], target: Long): Option[(Long,Long)] = 

    @tailrec
    def next(d: Seq[Long], accum: Long): Option[Int] =
      if (accum == target)
        Some(d.size) // return how many elements were left
      else if (accum > target)
        None
      else if (data.isEmpty)
        None
      else next(d.tail, accum + d.head)

    val pos = next(data, 0L).map(i => data.dropRight(i))
    pos.map(s => (s.min, s.max))

  @tailrec
  def findContiguousRange(range: Seq[Long], target: Long): Option[(Long, Long)] =
    if (range.isEmpty)
      None
    else
      val slice = findSliceSum(range, target)
      if (slice.isDefined)
        slice
      else
        findContiguousRange(range.tail, target)