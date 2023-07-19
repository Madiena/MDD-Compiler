package Utils

import java.io.File
import scala.io.{BufferedSource, Source}
import scala.sys.exit

class Reader {

  def readInput(): String = {
    var file: BufferedSource = null
    var input: String = ""
    if(new File("input.txt").exists()) {
      file = Source.fromFile("input.txt")
      for (line <- file.getLines()) {
        input = input + "\n" + line
      }
      file.close()
    } else {
      println("Input file could not be found.")
      exit(99)
    }
    if(input.isEmpty) {
      println("Input file is empty.")
      exit(99)
    }else {
      input = input.replace(input, input.substring(1))
      input
    }
  }
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
