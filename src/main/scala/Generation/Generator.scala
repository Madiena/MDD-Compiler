package Generation

import Utils.Reader

import java.io.File
import scala.sys.exit

class Generator extends WebsiteParser {
    val reader = new Reader

  def generateWebsite():Unit = {
      parseAll(website, reader.readInput()) match {
      case Success(matched, _) =>
        var nr = 1
        while (new File("file" + nr + ".html").exists()) {
          val f: File = new File("file" + nr + ".html")
          f.delete()
          nr += 1
        }
        matched.buildWebsite()
        if (!(new File("file1.html").exists() && new File("file2.html").exists())) {
          println("Error: File creation failed.\n")
          exit(99)
        }
    }
  }
}
