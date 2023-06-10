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

    parseAll(navLink, "(Link: (Startseite), (index.html))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<li><a href=\"index.html\">Startseite</a></li>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(navLink, "(Link: (Startseite), (index.html))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<li><a href=\"index.html\">Startseite</a></li>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(link, "(Link: (Startseite), (index.html))") match {
      case Success(matched, _) => assert(matched.toHtml == "<a href=\"index.html\">Startseite</a>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(navbarList, "(Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html)))") match {
      case Success(matched, _) => assert(matched.toHtml == "<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(text, "(Text: (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n<h4>Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n</div>\n</div>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(headline, "(Headline 4: (Kurzbeschreibung:))") match {
      case Success(matched, _) => assert(matched.toHtml == "<h4>Kurzbeschreibung:</h4>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(paragraph, "(Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.))") match {
      case Success(matched, _) => assert(matched.toHtml == "<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(unorderedList, "(List unordered: " +
      "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.)," +
      "(Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.)," +
      "(Skiena; S.: The Algorithm Design Manual. Springer.)," +
      "(Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.)" +
      ")") match {
      case Success(matched, _) => assert(matched.toHtml == "<div class=\"col-sm-8 text-left bg-content\">\n<ul><li>Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.</li>\n<li>Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.</li>\n<li>Skiena; S.: The Algorithm Design Manual. Springer.</li>\n<li>Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.</li>\n</ul></div>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(orderedList, "(List ordered: " +
      "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.)," +
      "(Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.)," +
      "(Skiena; S.: The Algorithm Design Manual. Springer.)," +
      "(Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.)" +
      ")") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"col-sm-8 text-left bg-content\">\n<ol><li>Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.</li>\n<li>Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.</li>\n<li>Skiena; S.: The Algorithm Design Manual. Springer.</li>\n<li>Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.</li>\n</ol></div>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(listElement, "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.)") match {
      case Success(matched, _) => assert(matched.toHtml == "<li>Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.</li>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(fullTable, "(Table: " +
      "(Tablerow: (Zeit), (Montag), (Dienstag), (Mittwoch), (Donnerstag), (Freitag))," +
      "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei))," +
      "(Tablerow: (9:50-11:20), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei))," +
      "(Tablerow: (11:30-13:00), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei))," +
      "(Tablerow: (14:00-15:30), (Compilerbau Praktikum), (Compilerbau: A&O), (frei), (frei), (Projektmanagement 2))," +
      "(Tablerow: (15:45-17:15), (Algorithmen: Entw., Anal., Impl.), (Graphalgorithmen), (frei), (frei), (frei))," +
      "(Tablerow: (17:30-19:00), (frei), (Graphalgorithmen), (frei), (frei), (frei))" +
      ")") match {
      case Success(matched, _) => assert(matched.toHtml == "<div class=\"col-sm-10 text-center bg-content\">\n<table class=\"table\">\n<thead>\n<tr>\n<th class=\"text-center\">Zeit</th>\n<th class=\"text-center\">Montag</th>\n<th class=\"text-center\">Dienstag</th>\n<th class=\"text-center\">Mittwoch</th>\n<th class=\"text-center\">Donnerstag</th>\n<th class=\"text-center\">Freitag</th>\n</tr>\n</thead>\n<tr>\n<td>8:00-9:30</td>\n<td>Compilerbau Vorlesung</td>\n<td>frei</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>9:50-11:20</td>\n<td>Compilerbau Praktikum</td>\n<td>MDD</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>11:30-13:00</td>\n<td>Compilerbau Praktikum</td>\n<td>MDD</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>14:00-15:30</td>\n<td>Compilerbau Praktikum</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n<td>Projektmanagement 2</td>\n</tr>\n<tr>\n<td>15:45-17:15</td>\n<td>Algorithmen: Entw., Anal., Impl.</td>\n<td>Graphalgorithmen</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>17:30-19:00</td>\n<td>frei</td>\n<td>Graphalgorithmen</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n</table>\n</div>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(tableRowHead, "(Tablerow: (Zeit), (Montag), (Dienstag), (Mittwoch), (Donnerstag), (Freitag))") match {
      case Success(matched, _) => assert(matched.toHtml == "<thead>\n<tr>\n<th class=\"text-center\">Zeit</th>\n<th class=\"text-center\">Montag</th>\n<th class=\"text-center\">Dienstag</th>\n<th class=\"text-center\">Mittwoch</th>\n<th class=\"text-center\">Donnerstag</th>\n<th class=\"text-center\">Freitag</th>\n</tr>\n</thead>")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(tableData, "(8:00-9:30)") match {
      case Success(matched, _) => assert(matched.toHtml == "<td>8:00-9:30</td>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(tableRowData, "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei))") match {
      case Success(matched, _) => assert(matched.toHtml == "<tr>\n<td>8:00-9:30</td>\n<td>Compilerbau Vorlesung</td>\n<td>frei</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n</tr>")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(tablehead, "(Zeit)") match {
      case Success(matched, _) => assert(matched.toHtml == "<th class=\"text-center\">Zeit</th>\n")
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
    parseAll(label, "(Label: (Id: (fname)), (Vorname))") match {
      case Success(matched, _) => assert(matched.toHtml == "<label for=\"fname\">Vorname:</label>")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(textArea, "(Textarea: (Id: (subject)), (Placeholder: (Nachricht)))") match {
      case Success(matched, _) => assert(matched.toHtml == "<textarea style=\"margin-bottom: 50px\" id=\"subject\" class=\"form-control\" placeholder=\"Nachricht\" style=\"height:200px\"></textarea>\n")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
    parseAll(input, "(Input: (Id: (fname)), (Placeholder: (Vorname)))") match {
      case Success(matched, _) => assert(matched.toHtml == "<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">")
      case Failure(msg, _) => println("Neee, das war nix, weil: " + msg)
    }
  }
}