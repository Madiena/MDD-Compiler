package Parser

object TestSimpleParser extends SimpleParser {

    def main(args: Array[String]): Unit = {
      parse(freq, "johnny 121") match {
        case Success(matched,_) => println(matched)
        case Failure(msg,_) => println("Neee, das war nix, weil: " + msg)
        case Error(msg,_) =>("Jetzt ist aber was ganz kaputt gegangen. Das hast du gemacht: " + msg)
      }
    }
}
