package Generator

import Utils.Reader

import java.io.File
import scala.sys.exit

object TestWebsiteParser extends WebsiteParser {
  def main(args: Array[String]): Unit = {

    // --- Tests for each element of AST --- //

    parseAll(website,
      """Website:
    (Page: (Header: (Image:(misc/Logo_THM_MNI.png)), (Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html))), (Link: (Literaturempfehlungen), (literaturempfehlungen.html)), (Link: (Stundenplan), (stundenplan.html)) ) ),
       (Body: (Text: (Headline 1: (Betriebssysteme)), (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)), (Headline 4: (Studiengang:)), (Paragraph: (Informatik B.Sc., Ingenieur-Informatik B.Sc.)), (Headline 4: (Nächste Veranstaltung:)), (Paragraph: (Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr)))),
       (Footer: (Link: (Kontakt), (kontakt.html)), (Link: (Impressum), (impressum.html)))
    ),(Page: (Header: (Image:(misc/Logo_THM_MNI.png)), (Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html))), (Link: (Literaturempfehlungen), (literaturempfehlungen.html)), (Link: (Stundenplan), (stundenplan.html)) ) ),
        (Body: (Text: (Headline 1: (Betriebssysteme)), (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)), (Headline 4: (Studiengang:)), (Paragraph: (Informatik B.Sc., Ingenieur-Informatik B.Sc.)), (Headline 4: (Nächste Veranstaltung:)), (Paragraph: (Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr)))),
        (Footer: (Link: (Kontakt), (kontakt.html)), (Link: (Impressum), (impressum.html)))
      )""") match {
      case Success(matched, _) =>
        val reader: Reader = new Reader()
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
        val input: String = reader.readFile()
        assert(input == "\n<!DOCTYPE html>\n<html lang=\"de\">\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">\n<meta charset=\"utf-8\">\n<link rel=\"stylesheet\" href=\"misc/bootstrap.css\">\n<link rel=\"stylesheet\" href=\"misc/my.css\">\n<script src=\"misc/jquery.js\"></script>\n<script src=\"misc/bootstrap.js\"></script>\n</head>\n<header>\n<div class=\"jumbotron\">\n<div class=\"container text-left\">\n<img src=\"misc/Logo_THM_MNI.png\">\n</div>\n</div>\n<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n<li><a href=\"index.html\">Startseite</a></li>\n<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n<li><a href=\"literaturempfehlungen.html\">Literaturempfehlungen</a></li>\n<li><a href=\"stundenplan.html\">Stundenplan</a></li>\n</ul>\n</div>\n</div>\n</nav>\n</header>\n<body>\n<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n<h1>Betriebssysteme</h1>\n<h4>Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n<h4>Studiengang:</h4>\n<p style=\"margin-bottom: 25px\">Informatik B.Sc., Ingenieur-Informatik B.Sc.</p>\n<h4>Nächste Veranstaltung:</h4>\n<p style=\"margin-bottom: 25px\">Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr</p>\n</div>\n</div>\n</body>\n<footer class=\"container-fluid text-center\">\n<ul>\n<li>\n<a href=\"kontakt.html\">Kontakt</a>\n</li><li>\n<a href=\"impressum.html\">Impressum</a>\n</li></ul>\n</footer>\n</html>\n<!DOCTYPE html>\n<html lang=\"de\">\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">\n<meta charset=\"utf-8\">\n<link rel=\"stylesheet\" href=\"misc/bootstrap.css\">\n<link rel=\"stylesheet\" href=\"misc/my.css\">\n<script src=\"misc/jquery.js\"></script>\n<script src=\"misc/bootstrap.js\"></script>\n</head>\n<header>\n<div class=\"jumbotron\">\n<div class=\"container text-left\">\n<img src=\"misc/Logo_THM_MNI.png\">\n</div>\n</div>\n<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n<li><a href=\"index.html\">Startseite</a></li>\n<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n<li><a href=\"literaturempfehlungen.html\">Literaturempfehlungen</a></li>\n<li><a href=\"stundenplan.html\">Stundenplan</a></li>\n</ul>\n</div>\n</div>\n</nav>\n</header>\n<body>\n<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n<h1>Betriebssysteme</h1>\n<h4>Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n<h4>Studiengang:</h4>\n<p style=\"margin-bottom: 25px\">Informatik B.Sc., Ingenieur-Informatik B.Sc.</p>\n<h4>Nächste Veranstaltung:</h4>\n<p style=\"margin-bottom: 25px\">Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr</p>\n</div>\n</div>\n</body>\n<footer class=\"container-fluid text-center\">\n<ul>\n<li>\n<a href=\"kontakt.html\">Kontakt</a>\n</li><li>\n<a href=\"impressum.html\">Impressum</a>\n</li></ul>\n</footer>\n</html>")
        println("✓ Website parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(page,
      """(Page: (Header: (Image:(misc/Logo_THM_MNI.png)), (Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html))), (Link: (Literaturempfehlungen), (literaturempfehlungen.html)), (Link: (Stundenplan), (stundenplan.html)) ) ),
                          (Body: (Text: (Headline 1: (Betriebssysteme)), (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)), (Headline 4: (Studiengang:)), (Paragraph: (Informatik B.Sc., Ingenieur-Informatik B.Sc.)), (Headline 4: (Nächste Veranstaltung:)), (Paragraph: (Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr)))),
                          (Footer: (Link: (Kontakt), (kontakt.html)), (Link: (Impressum), (impressum.html)))
                           )""") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<!DOCTYPE html>\n<html lang=\"de\">\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">\n<meta charset=\"utf-8\">\n<link rel=\"stylesheet\" href=\"misc/bootstrap.css\">\n<link rel=\"stylesheet\" href=\"misc/my.css\">\n<script src=\"misc/jquery.js\"></script>\n<script src=\"misc/bootstrap.js\"></script>\n</head>\n<header>\n<div class=\"jumbotron\">\n<div class=\"container text-left\">\n<img src=\"misc/Logo_THM_MNI.png\">\n</div>\n</div>\n<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n<li><a href=\"index.html\">Startseite</a></li>\n<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n<li><a href=\"literaturempfehlungen.html\">Literaturempfehlungen</a></li>\n<li><a href=\"stundenplan.html\">Stundenplan</a></li>\n</ul>\n</div>\n</div>\n</nav>\n</header>\n<body>\n<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n<h1>Betriebssysteme</h1>\n<h4>Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n<h4>Studiengang:</h4>\n<p style=\"margin-bottom: 25px\">Informatik B.Sc., Ingenieur-Informatik B.Sc.</p>\n<h4>Nächste Veranstaltung:</h4>\n<p style=\"margin-bottom: 25px\">Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr</p>\n</div>\n</div>\n</body>\n<footer class=\"container-fluid text-center\">\n<ul>\n<li>\n<a href=\"kontakt.html\">Kontakt</a>\n</li><li>\n<a href=\"impressum.html\">Impressum</a>\n</li></ul>\n</footer>\n</html>")
        println("✓ Page parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(header, "(Header: (Image:(misc/Logo_THM_MNI.png)), (Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Projekte), (Link: (Projekt: Dokumentationssoftware für Arztpraxen), (projekt1.html)), (Link: (Projekt: Erweiterung von Datalog um die Berechnung von Rängen), (projekt2.html))), (Link: (Literaturempfehlungen), (literaturempfehlungen.html)), (Link: (Stundenplan), (stundenplan.html))))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<header>\n<div class=\"jumbotron\">\n<div class=\"container text-left\">\n<img src=\"misc/Logo_THM_MNI.png\">\n</div>\n</div>\n<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n<li><a href=\"index.html\">Startseite</a></li>\n<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Projekte\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"projekt1.html\">Projekt: Dokumentationssoftware für Arztpraxen</a></li>\n<li><a href=\"projekt2.html\">Projekt: Erweiterung von Datalog um die Berechnung von Rängen</a></li>\n</ul>\n</li>\n<li><a href=\"literaturempfehlungen.html\">Literaturempfehlungen</a></li>\n<li><a href=\"stundenplan.html\">Stundenplan</a></li>\n</ul>\n</div>\n</div>\n</nav>\n</header>\n")
        println("✓ Header parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(body, "(Body: (Text: (Headline 1: (Betriebssysteme)), (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)), (Headline 4: (Studiengang:)), (Paragraph: (Informatik B.Sc., Ingenieur-Informatik B.Sc.)), (Headline 4: (Nächste Veranstaltung:)), (Paragraph: (Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr))))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<body>\n<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n<h1>Betriebssysteme</h1>\n<h4>Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n<h4>Studiengang:</h4>\n<p style=\"margin-bottom: 25px\">Informatik B.Sc., Ingenieur-Informatik B.Sc.</p>\n<h4>Nächste Veranstaltung:</h4>\n<p style=\"margin-bottom: 25px\">Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr</p>\n</div>\n</div>\n</body>\n")
        println("✓ Body parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(footer, "(Footer: (Link: (Kontakt), (kontakt.html)), (Link: (Impressum), (impressum.html)))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<footer class=\"container-fluid text-center\">\n<ul>\n<li>\n<a href=\"kontakt.html\">Kontakt</a>\n</li><li>\n<a href=\"impressum.html\">Impressum</a>\n</li></ul>\n</footer>\n")
        println("✓ Footer parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(image, "(Image:(misc/Logo_THM_MNI.png))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<img src=\"misc/Logo_THM_MNI.png\">\n")
        println("✓ Image parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    // Full navbar
    parseAll(navbar,
      """(Navbar:
        |(Link: (Startseite), (index.html)),
        |(Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html))),
        |(Dropdown: (Projekte), (Link: (Projekt: Dokumentationssoftware für Arztpraxen), (projekt1.html)), (Link: (Projekt: Erweiterung von Datalog um die Berechnung von Rängen), (projekt2.html))),
        |(Link: (Literaturempfehlungen), (literaturempfehlungen.html)),
        |(Link: (Stundenplan), (stundenplan.html))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n<li><a href=\"index.html\">Startseite</a></li>\n<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n" +
          "<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Projekte\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"projekt1.html\">Projekt: Dokumentationssoftware für Arztpraxen</a></li>\n<li><a href=\"projekt2.html\">Projekt: Erweiterung von Datalog um die Berechnung von Rängen</a></li>\n</ul>\n</li>\n" +
          "<li><a href=\"literaturempfehlungen.html\">Literaturempfehlungen</a></li>\n<li><a href=\"stundenplan.html\">Stundenplan</a></li>\n</ul>\n</div>\n</div>\n</nav>\n")
        println("✓ Navbar parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    // Navbar with only one dropdown
    parseAll(navbar, "(Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html))))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n<li><a href=\"index.html\">Startseite</a></li>\n<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n</ul>\n</div>\n</div>\n</nav>\n")
        println("✓ Navbar parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(navbarList, "(Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html)))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n")
        println("✓ Navbarlist parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(navLink, "(Link: (Startseite), (index.html))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<li><a href=\"index.html\">Startseite</a></li>\n")
        println("✓ Navlink parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(link, "(Link: (Startseite), (index.html))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<a href=\"index.html\">Startseite</a>\n")
        println("✓ Link parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(text, "(Text: (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n<h4>Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n</div>\n</div>\n")
        println("✓ Text parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(headline, "(Headline 4: (Kurzbeschreibung:))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<h4>Kurzbeschreibung:</h4>\n")
        println("✓ Headline parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(paragraph, "(Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n")
        println("✓ Paragraph parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(unorderedList, "(List unordered: " +
      "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.)," +
      "(Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.)," +
      "(Skiena; S.: The Algorithm Design Manual. Springer.)," +
      "(Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.)" +
      ")") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"col-sm-8 text-left bg-content\">\n<ul><li>Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.</li>\n<li>Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.</li>\n<li>Skiena; S.: The Algorithm Design Manual. Springer.</li>\n<li>Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.</li>\n</ul></div>\n")
        println("✓ Unordered list parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(orderedList, "(List ordered: " +
      "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.)," +
      "(Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.)," +
      "(Skiena; S.: The Algorithm Design Manual. Springer.)," +
      "(Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.)" +
      ")") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"col-sm-8 text-left bg-content\">\n<ol><li>Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.</li>\n<li>Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.</li>\n<li>Skiena; S.: The Algorithm Design Manual. Springer.</li>\n<li>Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.</li>\n</ol></div>\n")
        println("✓ Ordered list parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(listElement, "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.)") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<li>Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.</li>\n")
        println("✓ List element parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(fullTable, "(Table: " +
      "(Tablerow: (Zeit), (Montag), (Dienstag), (Mittwoch), (Donnerstag), (Freitag)), " +
      "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei)), " +
      "(Tablerow: (9:50-11:20), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei)), " +
      "(Tablerow: (11:30-13:00), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei)), " +
      "(Tablerow: (14:00-15:30), (Compilerbau Praktikum), (Compilerbau: A&O), (frei), (frei), (Projektmanagement 2)), " +
      "(Tablerow: (15:45-17:15), (Algorithmen: Entw., Anal., Impl.), (Graphalgorithmen), (frei), (frei), (frei)), " +
      "(Tablerow: (17:30-19:00), (frei), (Graphalgorithmen), (frei), (frei), (frei))" +
      ")") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"col-sm-10 text-center bg-content\">\n<table class=\"table\">\n<thead>\n<tr>\n<th class=\"text-center\">Zeit</th>\n<th class=\"text-center\">Montag</th>\n<th class=\"text-center\">Dienstag</th>\n<th class=\"text-center\">Mittwoch</th>\n<th class=\"text-center\">Donnerstag</th>\n<th class=\"text-center\">Freitag</th>\n</tr>\n</thead>\n<tr>\n<td>8:00-9:30</td>\n<td>Compilerbau Vorlesung</td>\n<td>frei</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>9:50-11:20</td>\n<td>Compilerbau Praktikum</td>\n<td>MDD</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>11:30-13:00</td>\n<td>Compilerbau Praktikum</td>\n<td>MDD</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>14:00-15:30</td>\n<td>Compilerbau Praktikum</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n<td>Projektmanagement 2</td>\n</tr>\n<tr>\n<td>15:45-17:15</td>\n<td>Algorithmen: Entw., Anal., Impl.</td>\n<td>Graphalgorithmen</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>17:30-19:00</td>\n<td>frei</td>\n<td>Graphalgorithmen</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n</table>\n</div>\n")
        println("✓ Table parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(tableRowHead, "(Tablerow: (Zeit), (Montag), (Dienstag), (Mittwoch), (Donnerstag), (Freitag))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<thead>\n<tr>\n<th class=\"text-center\">Zeit</th>\n<th class=\"text-center\">Montag</th>\n<th class=\"text-center\">Dienstag</th>\n<th class=\"text-center\">Mittwoch</th>\n<th class=\"text-center\">Donnerstag</th>\n<th class=\"text-center\">Freitag</th>\n</tr>\n</thead>")
        println("✓ Tablerow of tableheads parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(tableData, "(8:00-9:30)") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<td>8:00-9:30</td>\n")
        println("✓ Tabledata parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(tableRowData, "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<tr>\n<td>8:00-9:30</td>\n<td>Compilerbau Vorlesung</td>\n<td>frei</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n</tr>")
        println("✓ Tablerow of tabledatas parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(tablehead, "(Zeit)") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<th class=\"text-center\">Zeit</th>\n")
        println("✓ Tablehead parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    // Form with one input field
    parseAll(form, "(Form: " +
      "(Label: (Id: (fname)), (Vorname)), (Input: (Id: (fname)), (Placeholder: (Vorname)))" +
      ")") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">\n<label for=\"fname\">Vorname</label>\n\n<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">\n<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>")
        println("✓ Form with one input field parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    // Form with two input fields
    parseAll(form, "(Form: " +
      "(Label: (Id: (fname)), (Vorname)), (Input: (Id: (fname)), (Placeholder: (Vorname)))," +
      "(Label: (Id: (lname)), (Vorname)), (Input: (Id: (lname)), (Placeholder: (Nachname)))" +
      ")") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">\n<label for=\"fname\">Vorname</label>\n\n<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">\n<label for=\"lname\">Vorname</label>\n\n<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"lname\" placeholder=\"Nachname\">\n<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>")
        println("✓ Form with two input fields parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    // Form with two textareas
    parseAll(form, "(Form: " +
      "(Label: (Id: (fname)), (Vorname)), (Textarea: (Id: (fname)), (Placeholder: (Vorname)))," +
      "(Label: (Id: (lname)), (Vorname)), (Textarea: (Id: (lname)), (Placeholder: (Nachname)))" +
      ")") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">\n<label for=\"fname\">Vorname</label>\n\n<textarea style=\"margin-bottom: 50px\" id=\"fname\" class=\"form-control\" placeholder=\"Vorname\" style=\"height:200px\"></textarea>\n\n<label for=\"lname\">Vorname</label>\n\n<textarea style=\"margin-bottom: 50px\" id=\"lname\" class=\"form-control\" placeholder=\"Nachname\" style=\"height:200px\"></textarea>\n\n<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>")
        println("✓ Form with two textareas parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    // Form with one input field and one textarea
    parseAll(form, "(Form: " +
      "(Label: (Id: (fname)), (Vorname)), (Input: (Id: (fname)), (Placeholder: (Vorname)))," +
      "(Label: (Id: (subject)), (Vorname)),  (Textarea: (Id: (subject)), (Placeholder: (Nachricht)))" +
      ")") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">\n<label for=\"fname\">Vorname</label>\n\n<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">\n<label for=\"subject\">Vorname</label>\n\n<textarea style=\"margin-bottom: 50px\" id=\"subject\" class=\"form-control\" placeholder=\"Nachricht\" style=\"height:200px\"></textarea>\n\n<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>")
        println("✓ Form with one input field and one textarea parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(label, "(Label: (Id: (fname)), (Vorname:))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<label for=\"fname\">Vorname:</label>\n")
        println("✓ Label parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(textArea, "(Textarea: (Id: (subject)), (Placeholder: (Nachricht)))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<textarea style=\"margin-bottom: 50px\" id=\"subject\" class=\"form-control\" placeholder=\"Nachricht\" style=\"height:200px\"></textarea>\n")
        println("✓ Textarea parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
    parseAll(input, "(Input: (Id: (fname)), (Placeholder: (Vorname)))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">")
        println("✓ Input parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }


    // --- Test for semantic analysis and correct syntax --- //

    // Tests an incorrect syntax. This test is supposed to fail.
    parseAll(input, "(Input: Id: (fname), (Placeholder: (Vorname)))") match {
      case Success(matched, _) =>
        assert(matched.toHtml == "<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">")
        println("✓ Input parsed and generated correctly.\n")
      case Failure(msg, _) => println("Syntax Error: " + msg + "\n")
    }

    // Tests whether the same number of tabledatas are given for each row. This test is supposed to fail.
    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar: (Link: (Startseite), (index.html)))
        |), (Body: (Table: (Tablerow: (Zeit), (Montag)),
        |                  (Tablerow: (8:00-9:30))
        |           )
        |), (Footer: (Link: (Startseite), (index.html)))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: All table rows must have the same number as table columns!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Tests if navbar has too many elements. This test is supposed to fail.
    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar:  (Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)),
        |(Link: (Startseite), (index.html)))),
        | (Body: (Image:(misc/Logo_THM.png))),
        |(Footer: (Link: (Startseite), (index.html)))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: To provide an optimal overview, the navbar may only contain 10 elements or less.\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Test whether label id and input id are same. This test is supposed to fail.
    parseAll(website,
      """Website:
    (Page:
    (Header:
    (Image:(misc/Logo_THM.png)),
    (Navbar:  (Link: (Startseite), (index.html)),
    (Link: (Startseite), (index.html)))),
    (Body: (Form: (Label: (Id: (wrongId)), (Vorname)), (Input: (Id: (fname)), (Placeholder: (Vorname))))),
    (Footer: (Link: (Startseite), (index.html)))
    )""") match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: Label and form identifier must match!\n")
        println("Input field: " + matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Test whether label id and textarea id are same. This test is supposed to fail.
    parseAll(website,
      """Website:
    (Page:
    (Header:
    (Image:(misc/Logo_THM.png)),
    (Navbar:  (Link: (Startseite), (index.html)),
    (Link: (Startseite), (index.html)))),
    (Body: (Form: (Label: (Id: (wrongId)), (Vorname)), (Textarea: (Id: (fname)), (Placeholder: (Vorname))))),
    (Footer: (Link: (Startseite), (index.html)))
    )""") match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: Label and form identifier must match!\n")
        println("Textarea: " + matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Test whether pages are empty. This test is supposed to fail.
    parseAll(website,
      """Website: """) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one page must be provided!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Test a form is empty. This test is supposed to fail.
    parseAll(website,
      """Website:
    (Page:
    (Header:
    (Image:(misc/Logo_THM.png)),
    (Navbar:  (Link: (Startseite), (index.html)),
    (Link: (Startseite), (index.html)))),
    (Body: (Form: )),
    (Footer: (Link: (Startseite), (index.html)))
    )""") match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one form element must be provided within a form!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Tests whether a table is empty. This test is supposed to fail.
    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar: (Link: (Startseite), (index.html)))
        |), (Body: (Table: (Tablerow: ),
        |           )
        |), (Footer: (Link: (Startseite), (index.html)))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one row of table heads and one row of table datas must be provided within a table.\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Tests whether a text is empty. This test is supposed to fail.
    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar: (Link: (Startseite), (index.html)))
        |), (Body: (Text: )
        |), (Footer: (Link: (Startseite), (index.html)))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one text element must be provided within a text!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Tests whether an unordered list is empty. This test is supposed to fail.
    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar: (Link: (Startseite), (index.html)))
        |), (Body: (List unordered: )
        |), (Footer: (Link: (Startseite), (index.html)))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one list element must be provided within the unordered list!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Tests whether an ordered list is empty. This test is supposed to fail.
    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar: (Link: (Startseite), (index.html)))
        |), (Body: (List ordered: )
        |), (Footer: (Link: (Startseite), (index.html)))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one list element must be provided within the ordered list!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar: )
        |), (Body: (Image:(misc/Logo_THM.png))
        |), (Footer: (Link: (Startseite), (index.html)))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one element must be provided within the navbar!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Tests whether a navbar list is empty. This test is supposed to fail.
    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar: (Dropdown: (leeres Dropdown), ))
        |), (Body: (Image:(misc/Logo_THM.png))
        |), (Footer: (Link: (Startseite), (index.html)))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one link must be provided within the navbar list!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Tests whether the body is empty. This test is supposed to fail.
    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar: (Link: (Startseite), (index.html)))
        |), (Body:
        |), (Footer: (Link: (Startseite), (index.html)))
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one element must be provided within the body!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }

    // Tests whether the footer is empty. This test is supposed to fail.
    parseAll(website,
      """Website:
        |(Page:
        |(Header:
        |(Image:(misc/Logo_THM.png)),
        |(Navbar: (Link: (Startseite), (index.html)))
        |), (Body: (Image:(misc/Logo_THM.png))
        |), (Footer: )
        |)""".stripMargin) match {
      case Success(matched, _) =>
        assert(matched.analyzeSemantics(true) == "Error: At least one link must be provided within the footer!\n")
        println(matched.analyzeSemantics(true))
      case Failure(msg, _) => println("Syntax Error: " + msg)
    }
  }

}