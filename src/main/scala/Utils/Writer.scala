package Utils

import java.io.{File, PrintWriter}

class Writer {
  def writeFile(content: String, nr: Int): Unit = {
    val file: File = new File("file" + nr + ".html")
    if (!file.createNewFile()) {
      throw new RuntimeException("Creating new file didn't work!")
    }
    val writer = new PrintWriter(file);
    writer.write(content)
    writer.close()
  }

  def writeFailure(content: String): Unit = {
    val file: File = new File("failure.txt")
    if (file.exists()) {
      file.delete()
    }
    val writer = new PrintWriter(file);
    writer.write(content)
    writer.close()
  }
}