import Discovering.Discoverer
import Generation.Generator

import scala.sys.exit

object Main {
  val gen = new Generator
  val disc = new Discoverer

  def main(args: Array[String]): Unit = {
    if(args.length == 0) {
      println("Given program arguments are invalid. Try --help for usage information.")
      exit(0)
    }
    if (args(0) == "--help") {
      println("Program arguments are: \n--gen\t\tGenerates HMTL Code from input.txt.\n--disc\t\tDiscovers HTML Code of all available generated html files.\n--help\t\tPrints usage information.")
      exit(0)
    } else if (args(0) == "--gen") {
      gen.generateWebsite()
      exit(0)
    } else if (args(0) == "--disc") {
      println(disc.discoverWebsite().toString)
      exit(0)
    } else {
      println("Given program arguments are invalid. Try --help for usage information.")
      exit(0)
    }
  }
}
