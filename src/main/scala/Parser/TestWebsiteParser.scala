package Parser

object TestWebsiteParser extends WebsiteParser {
  def main(args: Array[String]): Unit = {
   parseAll(website, "Website: (" +
      "   Page: (" +
      "       (Header: (Image: (image.jpg)), (Navbar: (Link:(Id),(Destin.html)))), (Body: (Image: (image.jpg))), (Footer: (Link:(Id),(Destin.html)))" +
      "       " +
      "       )" +
      "   )") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
      case Error(msg, _) => ("Jetzt ist aber was ganz kaputt gegangen. Das hast du gemacht: " + msg)
    }
   parse(header, "(Header: (Image: (image.jpg)), (Navbar: (Link:(Id),(Destin.html))))") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
      case Error(msg, _) => ("Jetzt ist aber was ganz kaputt gegangen. Das hast du gemacht: " + msg)
    }
    parse(body, "(Body: (Image: (image.jpg)))") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
      case Error(msg, _) => ("Jetzt ist aber was ganz kaputt gegangen. Das hast du gemacht: " + msg)
    }
    parse(footer, "(Footer: (Link:(Id),(Destin.html)))") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
      case Error(msg, _) => ("Jetzt ist aber was ganz kaputt gegangen. Das hast du gemacht: " + msg)
    }
    System.out.println("----------------------------------parsing done------------------------------------")
    parse(form, "(Form: (h), (Textarea: (g))") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
      case Error(msg, _) => ("Jetzt ist aber was ganz kaputt gegangen. Das hast du gemacht: " + msg)
    }
  }
}