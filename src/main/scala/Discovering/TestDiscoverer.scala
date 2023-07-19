package Discovering

object TestDiscoverer extends Discoverer {
  def main(args: Array[String]): Unit = {
    var s: String = ""
    var o: Object = new Object
    // Test Input
    input = "<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">"
    s = discover(input).toString
    assert(s == "(Input: (Id: (fname)), (Placeholder: (Vorname)))")
    println(s + "\n")

    // Test Textarea
    input = "<textarea style=\"margin-bottom: 50px\" id=\"subject\" class=\"form-control\" placeholder=\"Nachricht\"" +
      " style=\"height:200px\"></textarea>\n"
    s = discover(input).toString
    assert(s == "(Textarea: (Id: (subject)), (Placeholder: (Nachricht)))")
    println(s + "\n")

    // Test Label
    input = "<label for=\"fname\">Vorname:</label>\n"
    s = discover(input).toString
    assert(s == "(Label: (Id: (fname)), (Vorname:))")
    println(s + "\n")

    // Test Form
    input = "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">\n<label for=\"fname\">Vorname</label>\n\n<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">\n<label for=\"subject\">Vorname</label>\n\n<textarea style=\"margin-bottom: 50px\" id=\"subject\" class=\"form-control\" placeholder=\"Nachricht\" style=\"height:200px\"></textarea>\n\n<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>"
    s = discover(input).toString
    assert(s == "(Form: (Label: (Id: (fname)), (Vorname)), (Input: (Id: (fname)), (Placeholder: (Vorname))), (Label: (Id: (subject)), (Vorname)), (Textarea: (Id: (subject)), (Placeholder: (Nachricht)))" +
      ")")
    println(s + "\n")

    // Test Tablehead
    input = "<th class=\"text-center\">Zeit</th>\n"
    s = discover(input).toString
    assert(s == "(Zeit)")
    println(s + "\n")

    // Test TableRowData
    input = "<tr>\n<td>8:00-9:30</td>\n<td>Compilerbau Vorlesung</td>\n<td>frei</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n</tr>"
    s = discover(input).toString
    assert(s == "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei))")
    println(s + "\n")

    // Test Tabledata
    input = "<td>8:00-9:30</td>\n"
    s = discover(input).toString
    assert(s == "(8:00-9:30)")
    println(s + "\n")

    // Test TableRowHead
    input = "<thead>\n<tr>\n<th class=\"text-center\">Zeit</th>\n<th class=\"text-center\">Montag</th>\n<th class=\"text-center\">Dienstag</th>\n<th class=\"text-center\">Mittwoch</th>\n<th class=\"text-center\">Donnerstag</th>\n<th class=\"text-center\">Freitag</th>\n</tr>\n</thead>"
    s = discover(input).toString
    assert(s == "(Tablerow: (Zeit), (Montag), (Dienstag), (Mittwoch), (Donnerstag), (Freitag))")
    println(s + "\n")

    // Test Table
    input = "<div class=\"col-sm-10 text-center bg-content\">\n<table class=\"table\">\n<thead>\n<tr>\n<th class=\"text-center\">Zeit</th>\n<th class=\"text-center\">Montag</th>\n<th class=\"text-center\">Dienstag</th>\n<th class=\"text-center\">Mittwoch</th>\n<th class=\"text-center\">Donnerstag</th>\n<th class=\"text-center\">Freitag</th>\n</tr>\n</thead>\n<tr>\n<td>8:00-9:30</td>\n<td>Compilerbau Vorlesung</td>\n<td>frei</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>9:50-11:20</td>\n<td>Compilerbau Praktikum</td>\n<td>MDD</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>11:30-13:00</td>\n<td>Compilerbau Praktikum</td>\n<td>MDD</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>14:00-15:30</td>\n<td>Compilerbau Praktikum</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n<td>Projektmanagement 2</td>\n</tr>\n<tr>\n<td>15:45-17:15</td>\n<td>Algorithmen: Entw., Anal., Impl.</td>\n<td>Graphalgorithmen</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>17:30-19:00</td>\n<td>frei</td>\n<td>Graphalgorithmen</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n</table>\n</div>\n"
    s = discover(input).toString
    assert(s == "(Table: " +
      "(Tablerow: (Zeit), (Montag), (Dienstag), (Mittwoch), (Donnerstag), (Freitag)), " +
      "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei)), " +
      "(Tablerow: (9:50-11:20), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei)), " +
      "(Tablerow: (11:30-13:00), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei)), " +
      "(Tablerow: (14:00-15:30), (Compilerbau Praktikum), (Compilerbau: A&O), (frei), (frei), (Projektmanagement 2)), " +
      "(Tablerow: (15:45-17:15), (Algorithmen: Entw., Anal., Impl.), (Graphalgorithmen), (frei), (frei), (frei)), " +
      "(Tablerow: (17:30-19:00), (frei), (Graphalgorithmen), (frei), (frei), (frei))" +
      ")")
    println(s + "\n")

    // Test ListElement
    input = "<li>Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.</li>\n"
    s = discover(input).toString
    assert(s == "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.)")
    println(s + "\n")

    // Test OrderedList
    input = "<div class=\"col-sm-8 text-left bg-content\">\n<ol><li>Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.</li>\n<li>Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.</li>\n<li>Skiena; S.: The Algorithm Design Manual. Springer.</li>\n<li>Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.</li>\n</ol></div>\n"
    s = discover(input).toString
    assert(s == "(List ordered: " +
      "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.), " +
      "(Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.), " +
      "(Skiena; S.: The Algorithm Design Manual. Springer.), " +
      "(Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.)" +
      ")")
    println(s + "\n")

    // Test UnorderedList
    input = "<div class=\"col-sm-8 text-left bg-content\">\n<ul><li>Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.</li>\n<li>Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.</li>\n<li>Skiena; S.: The Algorithm Design Manual. Springer.</li>\n<li>Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.</li>\n</ul></div>\n"
    s = discover(input).toString
    assert(s == "(List unordered: " +
      "(Gogol-Döring, A.; Letschert, T.: Algorithmen für Dummies. Wiley.), " +
      "(Cormen, T. H.; Leiserson, C. E.; Rivest, R.; Stein, C.; Molitor, P.: Algorithmen. Eine Einführung. De Gruyter.), " +
      "(Skiena; S.: The Algorithm Design Manual. Springer.), " +
      "(Robert, Y.; Benoit, A.; Vivien, F.: A Guide To Algorithm Design. Paradigms, Methods, and Complexity Analysis. Chapman & Hall / CRC Press.)" +
      ")")
    println(s + "\n")

    // Test Paragraph
    input = "<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n"
    s = discover(input).toString
    assert(s == "(Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.))")
    println(s + "\n")

    // Test Headline
    input = "<h4>Kurzbeschreibung:</h4>\n"
    s = discover(input).toString
    assert(s == "(Headline 4: (Kurzbeschreibung:))")
    println(s + "\n")

    // Test Text
    input = "<h4>Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n"
    s = discover(input).toString
    assert(s == "(Text: (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)))")
    println(s + "\n")

    // Test Link
    input = "<div><a href=\"index.html\">Startseite</a>\n</div>\n"
    s = discover(input).toString
    assert(s == "(Link: (Startseite), (index.html))")
    println(s + "\n")

    // Test Navlink
    input = "<li><a href=\"index.html\">Startseite</a></li>\n"
    s = discover(input).toString
    assert(s == "(Link: (Startseite), (index.html))")
    println(s + "\n")

    // Test Navbarlist
    input = "<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n"
    s = discover(input).toString
    assert(s == "(Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html)))")
    println(s + "\n")

    // Test Navbar
    input = "<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n<li><a href=\"index.html\">Startseite</a></li>\n<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n</ul>\n</div>\n</div>\n</nav>\n"
    s = discover(input).toString
    assert(s == "(Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html))))")
    println(s + "\n")

    // Test Image
    input = "<img src=\"misc/Logo_THM_MNI.png\">\n"
    s = discover(input).toString
    assert(s == "(Image:(misc/Logo_THM_MNI.png))")
    println(s + "\n")

    // Test Footer
    input = "<footer class=\"container-fluid text-center\">\n<ul>\n<li>\n<div><a href=\"kontakt.html\">Kontakt</a>\n</div>\n</li><li>\n<div><a href=\"impressum.html\">Impressum</a>\n</div>\n</li></ul>\n</footer>\n"
    s = discover(input).toString
    assert(s == "(Footer: (Link: (Kontakt), (kontakt.html)), (Link: (Impressum), (impressum.html)))")
    println(s + "\n")

    // Test Body with text elements
    input = "<body>\n<div class=\"container-fluid text-center\">\n<div class=\"row content\">\n\n<div class=\"col-sm-2 sidenav\">\n\n</div>\n\n<div class=\"col-sm-8 text-left bg-content container\"><h1>Betriebssysteme</h1>\n<h4>Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n<h4>Studiengang:</h4>\n<p style=\"margin-bottom: 25px\">Informatik B.Sc., Ingenieur-Informatik B.Sc.</p>\n<h4>Nächste Veranstaltung:</h4>\n<p style=\"margin-bottom: 25px\">Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr</p>\n</div></div></div></body>\n";
    s = discover(input).toString
    assert(s == "(Body: (Text: (Headline 1: (Betriebssysteme)), (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)), (Headline 4: (Studiengang:)), (Paragraph: (Informatik B.Sc., Ingenieur-Informatik B.Sc.)), (Headline 4: (Nächste Veranstaltung:)), (Paragraph: (Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr))))")
    println(s + "\n")

    // Test Body with image and link
    input = "<body>\n<div class=\"container-fluid text-center\">\n<div class=\"row content\">\n\n<div class=\"col-sm-2 sidenav\">\n\n</div>\n\n<div class=\"col-sm-8 text-left bg-content container\"><img src=\"image.png\">\n<div><a href=\"index.html\">Startseite</a>\n</div>\n</div></div></div></body>\n"
    s = discover(input).toString
    println(s)
    assert(s == "(Body: (Image:(image.png)), (Link: (Startseite), (index.html)))")
    println(s + "\n")

    // Test Body with unordered list and ordered list
    input = "<body>\n<div class=\"container-fluid text-center\">\n<div class=\"row content\">\n\n<div class=\"col-sm-2 sidenav\">\n\n</div>\n\n<div class=\"col-sm-8 text-left bg-content container\"><div class=\"col-sm-8 text-left bg-content\">\n<ul><li>Ice Cream</li>\n<li>Cheese</li>\n<li>Rhubarb</li>\n</ul></div>\n<div class=\"col-sm-8 text-left bg-content\">\n<ol><li>Basil</li>\n<li>Avocado</li>\n<li>Lime</li>\n</ol></div>\n</div></div></div></body>\n"
    s = discover(input).toString
    assert(s == "(Body: (List unordered: (Ice Cream), (Cheese), (Rhubarb)), (List ordered: (Basil), (Avocado), (Lime)))")
    println(s + "\n")

    // Test Body with table
    input = "<body>\n<div class=\"container-fluid text-center\">\n<div class=\"row content\">\n\n<div class=\"col-sm-2 sidenav\">\n\n</div>\n\n<div class=\"col-sm-8 text-left bg-content container\"><div class=\"col-sm-10 text-center bg-content\">\n<table class=\"table\">\n<thead>\n<tr>\n<th class=\"text-center\">Zeit</th>\n<th class=\"text-center\">Montag</th>\n<th class=\"text-center\">Dienstag</th>\n<th class=\"text-center\">Mittwoch</th>\n<th class=\"text-center\">Donnerstag</th>\n<th class=\"text-center\">Freitag</th>\n</tr>\n</thead>\n<tr>\n<td>8:00-9:30</td>\n<td>Compilerbau Vorlesung</td>\n<td>frei</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>9:50-11:20</td>\n<td>Compilerbau Praktikum</td>\n<td>MDD</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>11:30-13:00</td>\n<td>Compilerbau Praktikum</td>\n<td>MDD</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>14:00-15:30</td>\n<td>Compilerbau Praktikum</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n<td>Projektmanagement 2</td>\n</tr>\n<tr>\n<td>15:45-17:15</td>\n<td>Algorithmen: Entw., Anal., Impl.</td>\n<td>Graphalgorithmen</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n<tr>\n<td>17:30-19:00</td>\n<td>frei</td>\n<td>Graphalgorithmen</td>\n<td>frei</td>\n<td>frei</td>\n<td>frei</td>\n</tr>\n</table>\n</div>\n</div></div></div></body>\n"
    s = discover(input).toString
    assert(s == "(Body: (Table: " +
      "(Tablerow: (Zeit), (Montag), (Dienstag), (Mittwoch), (Donnerstag), (Freitag)), " +
      "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei)), " +
      "(Tablerow: (9:50-11:20), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei)), " +
      "(Tablerow: (11:30-13:00), (Compilerbau Praktikum), (MDD), (frei), (frei), (frei)), " +
      "(Tablerow: (14:00-15:30), (Compilerbau Praktikum), (Compilerbau: A&O), (frei), (frei), (Projektmanagement 2)), " +
      "(Tablerow: (15:45-17:15), (Algorithmen: Entw., Anal., Impl.), (Graphalgorithmen), (frei), (frei), (frei)), " +
      "(Tablerow: (17:30-19:00), (frei), (Graphalgorithmen), (frei), (frei), (frei))" +
      "))")
    println(s + "\n")

    // Test Body with form
    input = "<body>\n<div class=\"container-fluid text-center\">\n<div class=\"row content\">\n\n<div class=\"col-sm-2 sidenav\">\n\n</div>\n\n<div class=\"col-sm-8 text-left bg-content container\"><div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">\n<label for=\"fname\">Vorname</label>\n\n<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">\n<label for=\"subject\">Vorname</label>\n\n<textarea style=\"margin-bottom: 50px\" id=\"subject\" class=\"form-control\" placeholder=\"Nachricht\" style=\"height:200px\"></textarea>\n\n<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div></div></div></div></body>\n"
    s = discover(input).toString
    assert(s == "(Body: (Form: (Label: (Id: (fname)), (Vorname)), (Input: (Id: (fname)), (Placeholder: (Vorname))), (Label: (Id: (subject)), (Vorname)), (Textarea: (Id: (subject)), (Placeholder: (Nachricht)))" +
      "))")
    println(s + "\n")

    // Test Header
    input = "<header>\n<div class=\"jumbotron\">\n<div class=\"container text-left\">\n<img src=\"misc/Logo_THM_MNI.png\"></div>\n</div>\n<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n<li><a href=\"index.html\">Startseite</a></li>\n<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Projekte\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"projekt1.html\">Projekt: Dokumentationssoftware für Arztpraxen</a></li>\n<li><a href=\"projekt2.html\">Projekt: Erweiterung von Datalog um die Berechnung von Rängen</a></li>\n</ul>\n</li>\n<li><a href=\"literaturempfehlungen.html\">Literaturempfehlungen</a></li>\n<li><a href=\"stundenplan.html\">Stundenplan</a></li>\n</ul>\n</div>\n</div>\n</nav>\n</header>\n"
    s = discover(input).toString
    assert(s == "(Header: (Image:(misc/Logo_THM_MNI.png)), (Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Projekte), (Link: (Projekt: Dokumentationssoftware für Arztpraxen), (projekt1.html)), (Link: (Projekt: Erweiterung von Datalog um die Berechnung von Rängen), (projekt2.html))), (Link: (Literaturempfehlungen), (literaturempfehlungen.html)), (Link: (Stundenplan), (stundenplan.html))))")
    println(s + "\n")

    // Test Page
    input = "<!DOCTYPE html>\n<html lang=\"de\">\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">\n<meta charset=\"utf-8\">\n<link rel=\"stylesheet\" href=\"misc/bootstrap.css\">\n<link rel=\"stylesheet\" href=\"misc/my.css\">\n<script src=\"misc/jquery.js\"></script>\n<script src=\"misc/bootstrap.js\"></script>\n</head>\n<header>\n<div class=\"jumbotron\">\n<div class=\"container text-left\">\n<img src=\"misc/Logo_THM_MNI.png\">\n</div>\n</div>\n<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n<li><a href=\"index.html\">Startseite</a></li>\n<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">Veranstaltungen\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n<li><a href=\"algorithmen.html\">Algorithmen</a></li>\n<li><a href=\"betriebssysteme.html\">Betriebssysteme</a></li>\n<li><a href=\"computergrafik.html\">Computergrafik</a></li>\n<li><a href=\"archiv.html\">Archiv</a></li>\n</ul>\n</li>\n<li><a href=\"literaturempfehlungen.html\">Literaturempfehlungen</a></li>\n<li><a href=\"stundenplan.html\">Stundenplan</a></li>\n</ul>\n</div>\n</div>\n</nav>\n</header>\n<body>\n<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n<h1>Betriebssysteme</h1>\n<h4>Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n<h4>Studiengang:</h4>\n<p style=\"margin-bottom: 25px\">Informatik B.Sc., Ingenieur-Informatik B.Sc.</p>\n<h4>Nächste Veranstaltung:</h4>\n<p style=\"margin-bottom: 25px\">Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr</p>\n</div>\n</div>\n</body>\n<footer class=\"container-fluid text-center\">\n<ul>\n<li>\n<a href=\"kontakt.html\">Kontakt</a>\n</li><li>\n<a href=\"impressum.html\">Impressum</a>\n</li></ul>\n</footer>\n</html>"
    s = discover(input).toString
    assert(s == "(Page: (Header: (Image:(misc/Logo_THM_MNI.png)), (Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html))), (Link: (Literaturempfehlungen), (literaturempfehlungen.html)), (Link: (Stundenplan), (stundenplan.html)))), (Body: (Text: (Headline 1: (Betriebssysteme)), (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)), (Headline 4: (Studiengang:)), (Paragraph: (Informatik B.Sc., Ingenieur-Informatik B.Sc.)), (Headline 4: (Nächste Veranstaltung:)), (Paragraph: (Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr)))), (Footer: (Link: (Kontakt), (kontakt.html)), (Link: (Impressum), (impressum.html))))")
    println(s + "\n")

    // Test Website
    s = discoverWebsite().toString
    assert(s == "Website: (Page: (Header: (Image:(misc/Logo_THM_MNI.png)), (Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html))), (Link: (Literaturempfehlungen), (literaturempfehlungen.html)), (Link: (Stundenplan), (stundenplan.html)))), (Body: (Text: (Headline 1: (Betriebssysteme)), (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)), (Headline 4: (Studiengang:)), (Paragraph: (Informatik B.Sc., Ingenieur-Informatik B.Sc.)), (Headline 4: (Nächste Veranstaltung:)), (Paragraph: (Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr)))), (Footer: (Link: (Kontakt), (kontakt.html)), (Link: (Impressum), (impressum.html)))), (Page: (Header: (Image:(misc/Logo_THM_MNI.png)), (Navbar: (Link: (Startseite), (index.html)), (Dropdown: (Veranstaltungen), (Link: (Algorithmen), (algorithmen.html)), (Link: (Betriebssysteme), (betriebssysteme.html)), (Link: (Computergrafik), (computergrafik.html)), (Link: (Archiv), (archiv.html))), (Link: (Literaturempfehlungen), (literaturempfehlungen.html)), (Link: (Stundenplan), (stundenplan.html)))), (Body: (Text: (Headline 1: (Betriebssysteme)), (Headline 4: (Kurzbeschreibung:)), (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)), (Headline 4: (Studiengang:)), (Paragraph: (Informatik B.Sc., Ingenieur-Informatik B.Sc.)), (Headline 4: (Nächste Veranstaltung:)), (Paragraph: (Montag, 24.04.2023: 08:00 Uhr - 09:30 Uhr)))), (Footer: (Link: (Kontakt), (kontakt.html)), (Link: (Impressum), (impressum.html))))")
    println(s + "\n")

    // Test discovering of partial incorrect input - this test contains html code including an invalid headline that
    // will be not be translated into the DSL.
    input = "<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n<h4Kurzbeschreibung:</h4>\n<p style=\"margin-bottom: 25px\">In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.</p>\n</div>\n</div>\n"
    s = discover(input).toString
    assert(s == "(Text: (Paragraph: (In der Veranstaltung werden Grundlagen der Rechnerarchitektur sowie Architektur, Funktionsweise und Programmierschnittstellen moderner Betriebssysteme behandelt und in praktischen Aufgaben exemplarisch vertieft.)))")
    println(s + "\n")
  }




}
