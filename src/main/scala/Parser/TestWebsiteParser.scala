package Parser

object TestWebsiteParser extends WebsiteParser {


  def main(args: Array[String]): Unit = {
    parse(statementendel, "(I am table datum),") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
      case Error(msg, _) => ("Jetzt ist aber was ganz kaputt gegangen. Das hast du gemacht: " + msg)
    }
  }
}
