package Utils

import java.io.FileWriter

object FileWriter {
  def main(args: Array[String]): Unit = {
    val filename = "src/writeThis.txt"
    val text = "This is Text for youuuu"

    val fileWriter = new FileWriter(filename);
    fileWriter.write(text)
    fileWriter.close
  }
}
