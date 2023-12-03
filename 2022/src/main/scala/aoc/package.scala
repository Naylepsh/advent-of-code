import scala.io.Source._

package object aoc {
  def getFileContent(path: String): Iterator[String] = 
    fromFile(path).getLines
}
