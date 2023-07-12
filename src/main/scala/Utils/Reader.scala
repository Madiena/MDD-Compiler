package Utils

import java.io.File
import scala.io.{BufferedSource, Source}

class Reader {
  def readFile(): String = {
    var file: BufferedSource = null
    var num = 1
    var input: String = ""
    while (new File("file" + num + ".html").exists()) {
      file = Source.fromFile("file" + num + ".html")
      for (line <- file.getLines()) {
        input = input + "\n" + line
      }
      file.close()
      num += 1
    }
    input = input.replace(input, input.substring(1))
    input
  }
}
