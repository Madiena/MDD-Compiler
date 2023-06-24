package Utils

import java.io.{File, PrintWriter}

class Writer {
  def writeFile(content: String): Unit = {
    var nr = 1
    val file: File = new File("file" + nr + ".html")
    if (!file.createNewFile()) {
      throw new RuntimeException("Creating new file didn't work!")
    }

    val writer = new PrintWriter(file);
    writer.write(content)
    writer.close()
  }
}










