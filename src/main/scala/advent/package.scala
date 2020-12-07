import scala.annotation.tailrec
import scala.util.{Try, Using}

package object advent {

  def getDataWithTransform[T](path: String)(transform: String => T) : Try[Seq[T]] = {
    Using(scala.io.Source.fromFile(path)) { _.getLines().map(transform).toSeq }
  }

  def getData(path: String): Try[Seq[String]] = getDataWithTransform[String](path)(identity[String])

  /**
   * In a lot of these exercises, input data is broken into groups of lines that are separated by
   * blank lines. Initially I used a quick and dirty method of pulling the whole input file as a single
   * string and splitting on "\n\n" and then splitting the groups... it was messy. This is cleaner
   * because it allows me to process the data without needing to create a single mega string in memory
   * first.
   * @param source An iterator of strings, like Source.fromFile.getLines
   * @return An iterator of groups of strings, each groups be represented by Seq[String]
   */
  def groupedIterator(source: Iterator[String]): Iterator[Seq[String]] = {

    @tailrec
    def groupAppender(group: List[String], s: Iterator[String]): List[String] = {
      if (! s.hasNext) {
        group.reverse
      } else {
        val next = s.next()
        if (next.isEmpty) {
          group.reverse
        } else {
          groupAppender(next :: group, s)
        }
      }
    }
    
    new Iterator[Seq[String]] {
      override def hasNext: Boolean = source.hasNext
      override def next(): Seq[String] = groupAppender(Nil, source)
    }
  }
  
  def getGroupsWithTransform[T](path: String)(transform: Seq[String] => T): Try[Seq[T]] = {
    Using(scala.io.Source.fromFile(path)) { s => groupedIterator(s.getLines).map(transform).toSeq }
  }
  
  def getGroups(path: String): Try[Seq[Seq[String]]] = getGroupsWithTransform[Seq[String]](path)(identity[Seq[String]])
  
  def toGrouped(data: String): Seq[Seq[String]] = groupedIterator(data.split("\n").iterator).toSeq
}
