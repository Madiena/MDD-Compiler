package Discovering

import Generation.Absyn._
import Utils.Reader

import scala.sys.exit


class Discoverer() {
  var input: String = ""
  var failure: String = ""

  def readInvalid(): Unit = {

  }

  def discoverWebsite(): Website = {
    val reader: Reader = new Reader()
    var pages: List[Page] = List()
    input = reader.readFile()
    var page: String = ""
    while (input != "") {
      while (!input.startsWith("\n<!DOCTYPE html") && input != "") {
        page = page + input.charAt(0)
        if (input.length > 1) {
          input = input.replace(input, input.substring(1))
        } else {
          input = ""
        }
      }
      val p = discoverPage(page)
      pages = pages ++ List(p)
      page = ""
      if (input != "") {
        input = input.replace(input, input.substring(1))
      }
    }
    val website: Website = Website(pages)
    println(website.toString + "\n")
    website
  }

  def discover(input: String): Object = {
    // Page
    if (input.contains("<!DOCTYPE html>")) {
      return discoverPage(input)
    } // Header
    else if (input.contains("<header>")) {
      return discoverHeader(input)
    } // Body
    else if (input.contains("<body>")) {
      return discoverBody(input)
    } // Footer
    else if (input.contains("<footer ")) {
      return discoverFooter(input)
    } // Form
    else if (input.contains("<form ")) {
      return discoverForm(input)
    } // Input
    else if (input.contains("<input")) {
      return discoverInput(input)
    } // Navbar
    else if (input.contains("<nav class=")) {
      return discoverNavbar(input)
    } // TextArea
    else if (input.contains("<textarea")) {
      return discoverTextarea(input)
    } // Label
    else if (input.contains("<label")) {
      return discoverLabel(input)
    } // Table
    else if (input.contains("<table")) {
      return discoverTable(input)
    } // TableRowHead
    else if (input.contains("<thead>")) {
      return discoverTableRowHead(input)
    } // Tablehead
    else if (input.contains("<th")) {
      return discoverTablehead(input)
    } // TableRowData
    else if (input.contains("<tr>")) {
      return discoverTableRowData(input)
    } // Tabledata
    else if (input.contains("<td>")) {
      return discoverTabledata(input)
    } // UnoreredList
    else if (input.contains("<ul>")) {
      return discoverUnorderedList(input)
    } // OrderedList
    else if (input.contains("<ol>")) {
      return discoverOrderedList(input)
    } // Navbarlist
    else if (input.contains("<li class=")) {
      return discoverNavbarlist(input)
    } // Navlink
    else if (input.contains("<li><a href")) {
      return discoverNavlink(input)
    } // ListElement
    else if (input.contains("<li>")) {
      return discoverListElement(input)
    } // Text
    else if (input.contains("<div class=\"col-sm-8 text-left bg-content\">\n<h") || input.contains("<div class=\"col-sm-8 text-left bg-content\">\n<p")) {
      return discoverText(input)
    } // Paragraph
    else if (input.contains("<p ")) {
      return discoverParagraph(input)
    } // Headline
    else if (input.contains("<h")) {
      return discoverHeadline(input)
    } // Link
    else if (input.contains("<a href")) {
      return discoverLink(input)
    } // Image
    else if (input.contains("<img ")) {
      return discoverImage(input)
    } else {
      println("Error: Given input doesn't match any possible language constructs.")
      exit(99)
    }
  }

  def discoverForm(input: String): Form = {
    if (input.substring(0, 156) != "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">\n") {
      failure = failure + input + "\n"
      return null
    }

    if(input.substring(input.length - 59, input.length) != "\n<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(156))
    val end = "<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>"
    var label: String = ""
    var formel: String = ""
    var formels: List[FormElEl] = List()
    while (sub != end) {
      while (!sub.startsWith("label>")) {
        label = label + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      for (i <- 0 to 6) {
        label = label + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      val l = discoverLabel(label)
      sub = sub.replace(sub, sub.substring(1))
      formel = formel + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
      while (!sub.startsWith("\n<label") && !sub.startsWith("\n<input type=\"submit\"")) {
        formel = formel + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      if (formel.startsWith("<input")) {
        val input = discoverInput(formel)
        val f = FormElEl(l, input)
        formels = formels ++ List(f)
      } else if (formel.startsWith("<textarea")) {
        val textArea = discoverTextarea(formel)
        val f = FormElEl(l, textArea)
        formels = formels ++ List(f)
      }
      label = ""
      formel = ""
      sub = sub.replace(sub, sub.substring(1))
    }
    val form = Form(formels)
    println(form + "\n")
    form
  }

  def discoverInput(input: String): InputEl = {
    if (input.substring(0, 72) != "<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(72))
    var id: String = ""
    var placeholder: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    if(sub.substring(0, 15) != "\" placeholder=\"") {
      failure = failure + input + "\n"
      return null
    }
    sub = sub.replace(sub, sub.substring(15))
    while (sub.charAt(0) != '"') {
      placeholder = placeholder + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val formIdentifier: FormIdentifier = FormIdentifier(id)
    val ph: Placeholder = Placeholder(placeholder)
    val inputEl: InputEl = InputEl(formIdentifier, ph)
    println(inputEl.toString + "\n")
    inputEl
  }

  def discoverTextarea(input: String): TextArea = {
    if (input.substring(0, 42) != "<textarea style=\"margin-bottom: 50px\" id=\"") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(42))
    var id: String = ""
    var placeholder: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    if (sub.substring(0, 36) != "\" class=\"form-control\" placeholder=\"") {
      failure = failure + input + "\n"
      return null
    }
    sub = sub.replace(sub, sub.substring(36))
    while (sub.charAt(0) != '"') {
      placeholder = placeholder + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val formIdentifier: FormIdentifier = FormIdentifier(id)
    val ph: Placeholder = Placeholder(placeholder)
    val textArea: TextArea = TextArea(formIdentifier, ph)
    println(textArea + "\n")
    textArea
  }

  def discoverLabel(input: String): Label = {
    if (input.substring(0, 12) != "<label for=\"") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(12))
    var id: String = ""
    var in: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    if (sub.substring(0, 2) != "\">") {
      failure = failure + input + "\n"
      return null
    }
    sub = sub.replace(sub, sub.substring(2))
    while (sub.charAt(0) != '<') {
      in = in + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val formIdentifier: FormIdentifier = FormIdentifier(id)
    val label: Label = Label(formIdentifier, in)
    println(label + "\n")
    label
  }

  def discoverTablehead(input: String): Tablehead = {
    if (input.substring(0, 24) != "<th class=\"text-center\">") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(24))
    var id: String = ""
    while (sub.charAt(0) != '<') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val tablehead: Tablehead = Tablehead(id)
    println(tablehead + "\n")
    tablehead
  }

  def discoverTableRowData(input: String): Tablerowdata = {
    if (input.substring(0, 5) != "<tr>\n") {
      failure = failure + input + "\n"
      return null
    }
    if (input.substring(input.length -6, input.length) != "\n</tr>") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(5))
    val end: String = "\n</tr>"
    var tabledata: String = ""
    var tabledatas: List[Tabledata] = List()
    while (sub != end) {
      if (sub.charAt(0) == '\n') {
        sub = sub.replace(sub, sub.substring(1))
      }
      while (sub.charAt(0) != '\n') {
        tabledata = tabledata + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      val td: Tabledata = discoverTabledata(tabledata)
      tabledatas = tabledatas ++ List(td)
      tabledata = ""
    }
    val tablerowdata: Tablerowdata = Tablerowdata(tabledatas)
    println(tablerowdata + "\n")
    tablerowdata
  }

  def discoverTabledata(input: String): Tabledata = {
    if (input.substring(0, 4) != "<td>") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(4))
    var id: String = ""
    while (sub.charAt(0) != '<') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val tabledata: Tabledata = Tabledata(id)
    println(tabledata)
    tabledata
  }

  def discoverTableRowHead(input: String): Tablerowhead = {
    if (input.substring(0, 13) != "<thead>\n<tr>\n") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(13))
    val end: String = "\n</tr>\n</thead>"
    if (input.substring(input.length - 15, input.length) != end) {
      failure = failure + input + "\n"
      return null
    }
    var tablehead: String = ""
    var tableheads: List[Tablehead] = List()
    while (sub != end) {
      if (sub.charAt(0) == '\n') {
        sub = sub.replace(sub, sub.substring(1))
      }
      while (sub.charAt(0) != '\n') {
        tablehead = tablehead + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      val th: Tablehead = discoverTablehead(tablehead)
      tableheads = tableheads ++ List(th)
      tablehead = ""
    }
    val tablerowhead: Tablerowhead = Tablerowhead(tableheads)
    println(tablerowhead + "\n")
    tablerowhead
  }

  def discoverTable(input: String): Table = {
    if (input.substring(0, 69) != "<div class=\"col-sm-10 text-center bg-content\">\n<table class=\"table\">\n") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(69))
    var trhString: String = ""
    val tableEnd: String = "\n</table>\n</div>\n"
    if (input.substring(input.length -17, input.length) != tableEnd) {
      failure = failure + input + "\n"
      return null
    }
    while (sub.contains("</thead>")) {
      trhString = trhString + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    for (i <- 1 to 7) {
      trhString = trhString + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val tablerowhead: Tablerowhead = discoverTableRowHead(trhString)
    var tablerowdata: String = ""
    var tableRowDatas: List[Tablerowdata] = List()
    while (sub != tableEnd) {
      if (sub.charAt(0) == '\n') {
        sub = sub.replace(sub, sub.substring(1))
      }
      while (!sub.startsWith("</tr>")) {
        tablerowdata = tablerowdata + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      for (i <- 1 to 5) {
        tablerowdata = tablerowdata + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      var trd: Tablerowdata = discoverTableRowData(tablerowdata)
      tableRowDatas = tableRowDatas ++ List(trd)
      tablerowdata = ""
    }
    val table: Table = Table(tablerowhead, tableRowDatas)
    println(table + "\n")
    table
  }

  def discoverListElement(input: String): ListElement = {
    if (input.substring(0, 4) != "<li>") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(4))
    var id: String = ""
    while (sub.charAt(0) != '<') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val listElement: ListElement = ListElement(id)
    print(listElement + "\n")
    listElement
  }

  def discoverOrderedList(input: String): OrderedList = {
    if (input.substring(0, 48) != "<div class=\"col-sm-8 text-left bg-content\">\n<ol>") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(48))
    val end: String = "\n</ol></div>\n"
    if (input.substring(input.length -13, input.length) != end) {
      failure = failure + input + "\n"
      return null
    }
    var listElement: String = ""
    var list: List[ListElement] = List()
    while (sub != end) {
      if (sub.charAt(0) == '\n') {
        sub = sub.replace(sub, sub.substring(1))
      }
      while (sub.charAt(0) != '\n') {
        listElement = listElement + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      var le: ListElement = discoverListElement(listElement)
      list = list ++ List(le)
      listElement = ""
    }
    val oList: OrderedList = OrderedList(list)
    println(oList + "\n")
    oList
  }

  def discoverUnorderedList(input: String): UnorderedList = {
    if (input.substring(0, 48) != "<div class=\"col-sm-8 text-left bg-content\">\n<ul>") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(48))
    val end: String = "\n</ul></div>\n"
    if (input.substring(input.length - 13, input.length) != end) {
      failure = failure + input + "\n"
      return null
    }
    var listElement: String = ""
    var list: List[ListElement] = List()
    while (sub != end) {
      if (sub.charAt(0) == '\n') {
        sub = sub.replace(sub, sub.substring(1))
      }
      while (sub.charAt(0) != '\n') {
        listElement = listElement + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      var le: ListElement = discoverListElement(listElement)
      list = list ++ List(le)
      listElement = ""
    }
    val uList: UnorderedList = UnorderedList(list)
    println(uList + "\n")
    uList
  }

  def discoverParagraph(input: String): Paragraph = {
    if (input.substring(0, 31) != "<p style=\"margin-bottom: 25px\">") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(31))
    var id: String = ""
    while (sub.charAt(0) != '<') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val paragraph: Paragraph = Paragraph(id)
    println(paragraph + "\n")
    paragraph
  }

  def discoverHeadline(input: String): Headline = {
    if (input.substring(0, 2) != "<h") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(2))
    val num: Char = sub.charAt(0)
    if (sub.substring(1, 2) != ">") {
      failure = failure + input + "\n"
      return null
    }
    sub = sub.replace(sub, sub.substring(2))
    var id: String = ""
    while (sub.charAt(0) != '<') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val headline: Headline = Headline(id, num.asDigit)
    println(headline + "\n")
    headline
  }

  def discoverText(input: String): Text = {
    if (input.substring(0, 124) != "<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(124))
    var headline: String = ""
    var paragraph: String = ""
    var textEls: List[TextEl] = List()
    val end: String = "</div>\n</div>\n"
    if (input.substring(input.length -14, input.length) != end) {
      failure = failure + input + "\n"
      return null
    }
    while (sub != end) {
      if (sub.charAt(0) == '\n') {
        sub = sub.replace(sub, sub.substring(1))
      }
      if (sub.charAt(1) == 'h') {
        headline = headline + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        while (sub.charAt(0) != '\n') {
          headline = headline + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        headline = headline + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        var hl: Headline = discoverHeadline(headline)
        textEls = textEls ++ List(hl)
      } else if (sub.charAt(1) == 'p') {
        paragraph = paragraph + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        while (sub.charAt(0) != '\n') {
          paragraph = paragraph + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        paragraph = paragraph + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        var pg: Paragraph = discoverParagraph(paragraph)
        textEls = textEls ++ List(pg)
      }
      headline = ""
      paragraph = ""
    }
    val text: Text = Text(textEls)
    println(text + "\n")
    text
  }

  def discoverLink(input: String): Link = {
    if (input.substring(0, 9) != "<a href=\"") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(9))
    var destination: String = ""
    var identifier: String = ""
    while (sub.charAt(0) != '"') {
      destination = destination + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    if (sub.substring(0, 2) != "\">") {
      failure = failure + input + "\n"
      return null
    }
    sub = sub.replace(sub, sub.substring(2))
    while (sub.charAt(0) != '<') {
      identifier = identifier + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val des: Destination = Destination(destination)
    val id: LinkIdentifier = LinkIdentifier(identifier)
    val link: Link = Link(des, id)
    println(link + "\n")
    link
  }

  def discoverNavlink(input: String): NavLink = {
    if (input.substring(0, 13) != "<li><a href=\"") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(13))
    var destination: String = ""
    var identifier: String = ""
    while (sub.charAt(0) != '"') {
      destination = destination + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    if (sub.substring(0, 2) != "\">") {
      failure = failure + input + "\n"
      return null
    }
    sub = sub.replace(sub, sub.substring(2))
    while (sub.charAt(0) != '<') {
      identifier = identifier + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val des: Destination = Destination(destination)
    val id: LinkIdentifier = LinkIdentifier(identifier)
    val link: NavLink = NavLink(des, id)
    println(link + "\n")
    link
  }

  def discoverNavbarlist(input: String): NavbarList = {
    if (input.substring(0, 72) != "<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(72))
    var id: String = ""
    val end: String = "</ul>\n</li>\n"
    if (input.substring(input.length -12, input.length) != end) {
      failure = failure + input + "\n"
      return null
    }
    var navlink: String = ""
    var navlinks: List[NavLink] = List()
    while (sub.charAt(0) != '\n') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    if (sub.substring(0, 60) != "\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n") {
      failure = failure + input + "\n"
      return null
    }
    sub = sub.replace(sub, sub.substring(60))
    while (sub != end) {
      if (sub.charAt(0) == '\n') {
        sub = sub.replace(sub, sub.substring(1))
      }
      while (sub.charAt(0) != '\n') {
        navlink = navlink + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      navlink = navlink + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
      var nl: NavLink = discoverNavlink(navlink)
      navlinks = navlinks ++ List(nl)
      navlink = ""
    }
    val navbarList: NavbarList = NavbarList(id, navlinks)
    println(navbarList + "\n")
    navbarList
  }

  def discoverNavbar(input: String): Navbar = {
    if (input.substring(0, 126) != "<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(126))
    val end: String = "</ul>\n</div>\n</div>\n</nav>\n"
    if (input.substring(input.length -27, input.length) != end) {
      failure = failure + input + "\n"
      return null
    }
    var navbarList: String = ""
    var navLink: String = ""
    var both: String = ""
    var navellist: List[NavbarElement] = List()
    while (sub != end) {
      if (sub.charAt(0) == '\n') {
        sub = sub.replace(sub, sub.substring(1))
      }
      for (i <- 0 to 2) {
        both = both + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      if (sub.charAt(0) == '>') {
        navLink = both
      } else {
        navbarList = both
      }
      while (sub.charAt(0) != '\n') {
        if (navLink.nonEmpty) {
          navLink = navLink + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        } else {
          navbarList = navbarList + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
      }
      if (navLink.nonEmpty) {
        navLink = navLink + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        var nl: NavLink = discoverNavlink(navLink)
        navellist = navellist ++ List(nl)
      } else {
        while (!sub.startsWith("</li>\n</ul>\n</li>\n")) {
          navbarList = navbarList + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        for (i <- 1 to 18) {
          navbarList = navbarList + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        var nlist: NavbarList = discoverNavbarlist(navbarList)
        navellist = navellist ++ List(nlist)
      }
      navbarList = ""
      navLink = ""
      both = ""
    }
    var navbar: Navbar = Navbar(navellist)
    println(navbar + "\n")
    navbar
  }

  def discoverImage(input: String): Image = {
    if (input.substring(0, 10) != "<img src=\"") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(10))
    var id: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val image: Image = Image(id)
    println(image + "\n")
    image
  }

  def discoverFooter(input: String): Footer = {
    if (input.substring(0, 55) != "<footer class=\"container-fluid text-center\">\n<ul>\n") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(55))
    var link: String = ""
    var links: List[Link] = List()
    val end: String = "\n</footer>\n"
    if (input.substring(input.length -11, input.length) != "<footer class=\"container-fluid text-center\">\n<ul>\n") {
      failure = failure + input + "\n"
      return null
    }
    while (sub != end) {
      while (!sub.startsWith("</li>")) {
        link = link + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      sub = sub.replace(sub, sub.substring(10))
      val l: Link = discoverLink(link)
      links = links ++ List(l)
      link = ""
    }
    val footer: Footer = Footer(links)
    println(footer + "\n")
    footer
  }

  def discoverBody(input: String): Body = {
    var sub: String = input.replace(input, input.substring(7))
    var bodyElements: List[BodyElement] = List()
    val end: String = "</body>\n"

    while (sub != end) {
      // image
      if (sub.startsWith("<img")) {
        var image: String = ""
        while (sub.charAt(0) != '\n') {
          image = image + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        image = image + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        val img: Image = discoverImage(image)
        bodyElements = bodyElements ++ List(img)
      }
      // link
      if (sub.startsWith("<a href")) {
        var link: String = ""
        while (sub.charAt(0) != '\n') {
          link = link + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        link = link + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        val l: Link = discoverLink(link)
        bodyElements = bodyElements ++ List(l)
      }
      // text elements
      if (sub.startsWith("<div class=\"container-fluid text-center")) {
        var textEl: String = ""
        while (!sub.startsWith("</div>\n</div>\n")) {
          textEl = textEl + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        for (i <- 0 to 13) {
          textEl = textEl + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        val te: Text = discoverText(textEl)
        bodyElements = bodyElements ++ List(te)
      }
      // unordered list
      if (sub.startsWith("<div class=\"col-sm-8 text-left bg-content\">\n<ul>")) {
        var list: String = ""
        while (!sub.startsWith("</ul></div>\n")) {
          list = list + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        for (i <- 0 to 11) {
          list = list + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        val unorderedList: UnorderedList = discoverUnorderedList(list)
        bodyElements = bodyElements ++ List(unorderedList)
      }
      // ordered list
      if (sub.startsWith("<div class=\"col-sm-8 text-left bg-content\">\n<ol>")) {
        var list: String = ""
        while (!sub.startsWith("</ol></div>\n")) {
          list = list + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        for (i <- 0 to 11) {
          list = list + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        val orderedList: OrderedList = discoverOrderedList(list)
        bodyElements = bodyElements ++ List(orderedList)
      }
      // table
      if (sub.startsWith("<div class=\"col-sm-10 text-center bg-content\">\n<table class=")) {
        var table: String = ""
        while (!sub.startsWith("</table>\n</div>\n")) {
          table = table + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        for (i <- 0 to 15) {
          table = table + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        val t: Table = discoverTable(table)
        bodyElements = bodyElements ++ List(t)
      }
      // form
      if (sub.startsWith("<div class=\"col-sm-8 text-left bg-content container\">\n<form action=")) {
        var form: String = ""
        while (!sub.startsWith("</form>\n</div>")) {
          form = form + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        for (i <- 0 to 13) {
          form = form + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        val f: Form = discoverForm(form)
        bodyElements = bodyElements ++ List(f)
      }
    }
    val body: Body = Body(bodyElements)
    println(body + "\n")
    body
  }

  def discoverHeader(input: String): Header = {
    var sub: String = input.replace(input, input.substring(67))
    var image: String = ""
    image = image + sub.charAt(0)
    sub = sub.replace(sub, sub.substring(1))
    while (sub.charAt(0) != '<') {
      image = image + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    sub = sub.replace(sub, sub.substring(14))
    val img: Image = discoverImage(image)
    var navbar: String = ""
    while (!sub.startsWith("</header>\n")) {
      navbar = navbar + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val nav: Navbar = discoverNavbar(navbar)
    val header: Header = Header(img, nav)
    println(header + "\n")
    header
  }

  def discoverPage(input: String): Page = {
    var sub: String = input.replace(input, input.substring(313))
    var header: String = ""
    var body: String = ""
    var footer: String = ""
    while (!sub.startsWith("</header>\n")) {
      header = header + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    for (i <- 0 to 9) {
      header = header + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val h: Header = discoverHeader(header)
    while (!sub.startsWith("</body>\n")) {
      body = body + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    for (i <- 0 to 7) {
      body = body + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val b: Body = discoverBody(body)
    while (sub != "</html>") {
      footer = footer + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val f: Footer = discoverFooter(footer)
    val page: Page = Page(h, b, f)
    println(page + "\n")
    page
  }

}
