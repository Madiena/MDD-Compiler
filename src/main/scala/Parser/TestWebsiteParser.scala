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


    parseAll(textArea, "(Textarea: (Id: (subject)), (Placeholder: (Nachricht)))") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }


    parseAll(link, "(Link: (Startseite), (index.html))") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }

    /*parseAll(fullTable, "(Table: " +
      "()" +
      ")") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }*/
    parseAll(listElement, "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.)") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(fullTable, "(Table: " +
      "(Tablerow: (Zeit), (Montag), (Dienstag), (Mittwoch), (Donnerstag), (Freitag))," +
      "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei))," +
      "(Tablerow: (9:50-11:20), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei))," +
      "(Tablerow: (11:30-13:00), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei))," +
      "(Tablerow: (14:00-15:30), (Compilerbau Praktikum), (Compilerbau A&O), (frei), (frei), (Projektmanagement 2))," +
      "(Tablerow: (15:45-17:15), (Algorithmen: Entw., Anal., Impl.), (Graphalgorithmen), (frei), (frei), (frei))," +
      "(Tablerow: (17:30-19:00), (frei), (Graphalgorithmen), (frei), (frei), (frei))" +
      ")") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(tableRowHead, "(Tablerow: (Zeit), (Montag), (Dienstag), (Mittwoch), (Donnerstag), (Freitag))") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(tableData, "(8:00-9:30)") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(tableRowData, "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei))") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(tablehead, "(Zeit)") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(form, "(Form: " +
      "((Label: (Id: (fname)), (Vorname)), (Input: (Id: (fname)), (Placeholder: (Vorname))))," +
      "((Label: (Id: (lname)), (Vorname)), (Input: (Id: (lname)), (Placeholder: (Nachname))))," +
      "((Textarea: (Id: (subject)), (Placeholder: (Nachricht))))" +
      ")") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(formEl, "((Label: (Id: (fname)), (Vorname)), (Input: (Id: (fname)), (Placeholder: (Vorname))))") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(label, "(Label: (Id: (fname)), (Vorname))") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(input, "(Input: (Id: (fname)), (Placeholder: (Vorname)))") match {
      case Success(matched, _) => println(matched.toHtml)
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
  }
}