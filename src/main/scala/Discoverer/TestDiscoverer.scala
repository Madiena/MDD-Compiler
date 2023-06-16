package Discoverer


object TestDiscoverer extends Discoverer {
  def main(args: Array[String]): Unit = {
    var s: String = ""
    // Test Input
    input = "<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"fname\" placeholder=\"Vorname\">"
    s = discover().toString
    assert(s == "(Input: (Id: (fname)), (Placeholder: (Vorname)))")

    // Test Textarea
    input = "<textarea style=\"margin-bottom: 50px\" id=\"subject\" class=\"form-control\" placeholder=\"Nachricht\"" +
      " style=\"height:200px\"></textarea>\n"
    s = discover().toString
    assert(s == "(Textarea: (Id: (subject)), (Placeholder: (Nachricht)))")

    // Test Label
    input = "<label for=\"fname\">Vorname:</label>\n"
    s = discover().toString
    assert(s == "(Label: (Id: (fname)), (Vorname:))")

    // Skip Form until it can be interpreted by Parser

    // Test Tablehead
    input = "<th class=\"text-center\">Zeit</th>\n"
    s = discover().toString
    assert(s == "(Zeit)")

    // Test TableRowData
    input = "<tr>\n<td>8:00-9:30</td>\n<td>Compilerbau Vorlesung</td>\n<td>frei</td>\n<td>Compilerbau: A&O</td>\n<td>frei</td>\n<td>frei</td>\n</tr>"
    s = discover().toString
    assert(s == "(Tablerow: (8:00-9:30), (Compilerbau Vorlesung), (frei), (Compilerbau: A&O), (frei), (frei))")

    // Test Tabledata
    input = "<td>8:00-9:30</td>\n"
    s = discover().toString
    assert(s == "(8:00-9:30)")
  }



}
