package Parser

object TestWebsiteParser extends WebsiteParser {
  def main(args: Array[String]): Unit = {
    parse(icon, "(http://wwww.3.org/2000/svg)") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
      case Error(msg, _) => ("Jetzt ist aber was ganz kaputt gegangen. Das hast du gemacht: " + msg)
    }
  }
}

/*



(Link: (Identifier), (Destination.html))," +
      "                   (Link: (Identifier), (Destination.html))

(Body: (Table: (Tablerow: (Tablehead), " +
      "                                          (Tablehead))," +
      "                               (Tablerow: (Tabledata)," +
      "                                          (Tabledata))))


      ,

      (Footer: (Link: (Identifier), (Destination.html))," +
      "                                    (Link: (Identifier), (Destination.html))))" +
      "

      //TODO: Check on Footer levels

 */