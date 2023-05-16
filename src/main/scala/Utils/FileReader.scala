package Utils

import scala.io.Source

object FileReader {
  def main(args: Array[String]): Unit = {
    val readText = Source.fromFile("src/readThis.txt").getLines.toList
    println(readText.mkString(" "))
  }
}
