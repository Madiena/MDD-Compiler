import Generation.Generator

object Main {
  val gen = new Generator
  def main(args: Array[String]): Unit = {
    gen.generateWebsite()
  }
}
