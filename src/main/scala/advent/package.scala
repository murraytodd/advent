import scala.util.{Try, Using}

package object advent {

  def getDataWithTransform[T](path: String)(transform: String => T) : Try[Seq[T]] = {
    Using(scala.io.Source.fromFile(path)) { _.getLines().map(transform).toSeq }
  }

  def getData(path: String): Try[Seq[String]] = getDataWithTransform[String](path)(identity[String])

}
