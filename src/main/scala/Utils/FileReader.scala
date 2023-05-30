package Utils

import scala.io.Source

object FileReader {
  def readFile(): Unit = {
    val readText = Source.fromFile("src/readThis.txt").getLines.toList
    println(readText.mkString(" "))
  }
}
