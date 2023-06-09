package Utils

import scala.io.Source

class Reader {
  def readFile(): Unit = {
    val readText = Source.fromFile("src/readThis.txt").getLines.toList
    println(readText.mkString(" "))
  }
}
