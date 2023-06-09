package Parser

object TestWebsiteParser extends WebsiteParser {
  def main(args: Array[String]): Unit = {
    /*parseAll(website, "Website: (Page: (" +
      "       (Header: (Image: (image.jpg)), (Navbar: (Link:(Id),(Destin.html))))," +
      "        (Body: (Image: (image.jpg))), (Footer: (Link:(Id),(Destin.html)))" +
      "       " +
      "       ))" +
      "") match {
      case Success(matched, _) => matched.buildWebsite()
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
      case Error(msg, _) => ("Jetzt ist aber was ganz kaputt gegangen. Das hast du gemacht: " + msg)
    }; parseAll(footer, "(Footer: (Link:(Id),(Destin.html)))") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }*/
    parseAll(form, "(Form:(Label: (Id: (fname)), (Vorname)),(Input: (Id: (fname)), (Vorname)),(Label: (Id: (lname)), (Nachname)),(Input: (Id: (lname)), (Nachname)),(Label: (Id: (subject)), (Ihre Nachricht)), (Textarea: (Id: (subject)), (Nachricht)))") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(label, "Label: (Id: (fname)), (Vorname)") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
  }
}