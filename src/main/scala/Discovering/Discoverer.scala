package Discovering

import Generation.Absyn._
import Utils.{Reader, Writer}

import scala.sys.exit


//noinspection ScalaUnusedSymbol,DuplicatedCode
class Discoverer() {
  var input: String = ""
  var failure: String = ""

  def discoverWebsite(): Website = {
    val writer: Writer = new Writer()
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
    writer.writeFailure(failure)
    website
  }

  def discover(input: String): Object = {
    val writer: Writer = new Writer
    var o: Object = new Object
    // Page
    if (input.contains("<!DOCTYPE html>")) {
      o = discoverPage(input)
      writer.writeFailure(failure)
      o
    } // Header
    else if (input.contains("<header>")) {
      o = discoverHeader(input)
      writer.writeFailure(failure)
      o
    } // Body
    else if (input.contains("<body>")) {
      o = discoverBody(input)
      writer.writeFailure(failure)
      o
    } // Footer
    else if (input.contains("<footer ")) {
      o = discoverFooter(input)
      writer.writeFailure(failure)
      o
    } // Form
    else if (input.contains("<form ")) {
      o = discoverForm(input)
      writer.writeFailure(failure)
      o
    } // Input
    else if (input.contains("<input")) {
      o = discoverInput(input)
      writer.writeFailure(failure)
      o
    } // Navbar
    else if (input.contains("<nav class=")) {
      o = discoverNavbar(input)
      writer.writeFailure(failure)
      o
    } // TextArea
    else if (input.contains("<textarea")) {
      o = discoverTextarea(input)
      writer.writeFailure(failure)
      o
    } // Label
    else if (input.contains("<label")) {
      o = discoverLabel(input)
      writer.writeFailure(failure)
      o
    } // Table
    else if (input.contains("<table")) {
      o = discoverTable(input)
      writer.writeFailure(failure)
      o
    } // TableRowHead
    else if (input.contains("<thead>")) {
      o = discoverTableRowHead(input)
      writer.writeFailure(failure)
      o
    } // Tablehead
    else if (input.contains("<th")) {
      o = discoverTablehead(input)
      writer.writeFailure(failure)
      o
    } // TableRowData
    else if (input.contains("<tr>")) {
      o = discoverTableRowData(input)
      writer.writeFailure(failure)
      o
    } // Tabledata
    else if (input.contains("<td>")) {
      o = discoverTabledata(input)
      writer.writeFailure(failure)
      o
    } // UnoreredList
    else if (input.contains("<ul>")) {
      o = discoverUnorderedList(input)
      writer.writeFailure(failure)
      o
    } // OrderedList
    else if (input.contains("<ol>")) {
      o = discoverOrderedList(input)
      writer.writeFailure(failure)
      o
    } // Navbarlist
    else if (input.contains("<li class=")) {
      o = discoverNavbarlist(input)
      writer.writeFailure(failure)
      o
    } // Navlink
    else if (input.contains("<li><a href")) {
      o = discoverNavlink(input)
      writer.writeFailure(failure)
      o
    } // ListElement
    else if (input.contains("<li>")) {
      o = discoverListElement(input)
      writer.writeFailure(failure)
      o
    } // Text
    else if (input.contains("<h") && input.contains("<p")) {
      o = discoverText(input)
      writer.writeFailure(failure)
      o
    } // Paragraph
    else if (input.contains("<p ")) {
      o = discoverParagraph(input)
      writer.writeFailure(failure)
      o
    } // Headline
    else if (input.contains("<h")) {
      o = discoverHeadline(input)
      writer.writeFailure(failure)
      o
    } // Link
    else if (input.contains("<a href")) {
      o = discoverLink(input)
      writer.writeFailure(failure)
      o
    } // Image
    else if (input.contains("<img ")) {
      o = discoverImage(input)
      writer.writeFailure(failure)
      o
    } else {
      println("Error: Given input doesn't match any possible language constructs.")
      exit(99)
    }
  }

  private def discoverForm(input: String): Form = {
    if (input.substring(0, 156) != "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">\n") {
      failure = failure + input + "\n"
      return null
    }

    if (input.substring(input.length - 59, input.length) != "\n<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>") {
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
        if (l != null && input != null) {
          val f = FormElEl(l, input)
          formels = formels ++ List(f)
        }
      } else if (formel.startsWith("<textarea")) {
        val textArea = discoverTextarea(formel)
        if (l != null && textArea != null) {
          val f = FormElEl(l, textArea)
          formels = formels ++ List(f)
        }
      }
      label = ""
      formel = ""
      sub = sub.replace(sub, sub.substring(1))
    }
    val form = Form(formels)
    form
  }

  private def discoverInput(input: String): InputEl = {
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
    if (sub.substring(0, 15) != "\" placeholder=\"") {
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
    inputEl
  }

  private def discoverTextarea(input: String): TextArea = {
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
    textArea
  }

  private def discoverLabel(input: String): Label = {
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
    label
  }

  private def discoverTablehead(input: String): Tablehead = {
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
    tablehead
  }

  private def discoverTableRowData(input: String): Tablerowdata = {
    if (input.substring(0, 5) != "<tr>\n") {
      failure = failure + input + "\n"
      return null
    }
    if (input.substring(input.length - 6, input.length) != "\n</tr>") {
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
      if (td != null) tabledatas = tabledatas ++ List(td)
      tabledata = ""
    }
    val tablerowdata: Tablerowdata = Tablerowdata(tabledatas)
    tablerowdata
  }

  private def discoverTabledata(input: String): Tabledata = {
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
    tabledata
  }

  private def discoverTableRowHead(input: String): Tablerowhead = {
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
      if (th != null) tableheads = tableheads ++ List(th)
      tablehead = ""
    }
    val tablerowhead: Tablerowhead = Tablerowhead(tableheads)
    tablerowhead
  }

  private def discoverTable(input: String): Table = {
    if (input.substring(0, 69) != "<div class=\"col-sm-10 text-center bg-content\">\n<table class=\"table\">\n") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(69))
    var trhString: String = ""
    val tableEnd: String = "\n</table>\n</div>\n"
    if (input.substring(input.length - 17, input.length) != tableEnd) {
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
      val trd: Tablerowdata = discoverTableRowData(tablerowdata)
      if (trd != null) tableRowDatas = tableRowDatas ++ List(trd)
      tablerowdata = ""
    }
    if (tablerowhead != null) {
      val table: Table = Table(tablerowhead, tableRowDatas)
      table
    } else {
      failure = failure + input + "\n"
      null
    }
  }

  private def discoverListElement(input: String): ListElement = {
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
    listElement
  }

  private def discoverOrderedList(input: String): OrderedList = {
    if (input.substring(0, 48) != "<div class=\"col-sm-8 text-left bg-content\">\n<ol>") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(48))
    val end: String = "\n</ol></div>\n"
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
      val le: ListElement = discoverListElement(listElement)
      if (le != null) list = list ++ List(le)
      listElement = ""
    }
    val oList: OrderedList = OrderedList(list)
    oList
  }

  private def discoverUnorderedList(input: String): UnorderedList = {
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
      val le: ListElement = discoverListElement(listElement)
      if (le != null) list = list ++ List(le)
      listElement = ""
    }
    val uList: UnorderedList = UnorderedList(list)
    uList
  }

  private def discoverParagraph(input: String): Paragraph = {
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
    paragraph
  }

  private def discoverHeadline(input: String): Headline = {
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
    headline
  }

  private def discoverText(input: String): Text = {
    var sub = input
    var headline: String = ""
    var paragraph: String = ""
    var textEls: List[TextEl] = List()
    while (sub.nonEmpty) {
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
        val hl: Headline = discoverHeadline(headline)
        if (hl != null) textEls = textEls ++ List(hl)
      } else if (sub.charAt(1) == 'p') {
        paragraph = paragraph + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        while (sub.charAt(0) != '\n') {
          paragraph = paragraph + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        paragraph = paragraph + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        val pg: Paragraph = discoverParagraph(paragraph)
        if (pg != null) textEls = textEls ++ List(pg)
      }
      headline = ""
      paragraph = ""
    }
    val text: Text = Text(textEls)
    text
  }

  private def discoverLink(input: String): Link = {
    if (input.substring(0, 14) != "<div><a href=\"") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(14))
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
    link
  }

  private def discoverNavlink(input: String): NavLink = {
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
    link
  }

  private def discoverNavbarlist(input: String): NavbarList = {
    if (input.substring(0, 72) != "<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(72))
    var id: String = ""
    val end: String = "</ul>\n</li>\n"
    if (input.substring(input.length - 12, input.length) != end) {
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
      val nl: NavLink = discoverNavlink(navlink)
      if (nl != null) navlinks = navlinks ++ List(nl)
      navlink = ""
    }
    val navbarList: NavbarList = NavbarList(id, navlinks)
    navbarList
  }

  private def discoverNavbar(input: String): Navbar = {
    if (input.substring(0, 126) != "<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n<ul class=\"nav navbar-nav\">\n") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(126))
    val end: String = "</ul>\n</div>\n</div>\n</nav>\n"
    if (input.substring(input.length - 27, input.length) != end) {
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
        val nl: NavLink = discoverNavlink(navLink)
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
        val nlist: NavbarList = discoverNavbarlist(navbarList)
        if (nlist != null) navellist = navellist ++ List(nlist)
      }
      navbarList = ""
      navLink = ""
      both = ""
    }
    val navbar: Navbar = Navbar(navellist)
    navbar
  }

  private def discoverImage(input: String): Image = {
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
    image
  }

  private def discoverFooter(input: String): Footer = {
    if (input.substring(0, 55) != "<footer class=\"container-fluid text-center\">\n<ul>\n<li>\n") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(55))
    var link: String = ""
    var links: List[Link] = List()
    val end: String = "\n</footer>\n"
    if (input.substring(input.length - 11, input.length) != end) {
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
      if (l != null) links = links ++ List(l)
      link = ""
    }
    val footer: Footer = Footer(links)
    footer
  }

  private def discoverBody(input: String): Body = {
    if (input.substring(0, 169) != "<body>\n<div class=\"container-fluid text-center\">\n<div class=\"row content\">\n\n<div class=\"col-sm-2 sidenav\">\n\n</div>\n\n<div class=\"col-sm-8 text-left bg-content container\">") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(169))
    var bodyElements: List[BodyElement] = List()
    val end: String = "</div></div></div></body>\n"

    if (input.substring(input.length - 26, input.length) != "</div></div></div></body>\n") {
      failure = failure + input + "\n"
      return null
    }
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
        if (img != null) bodyElements = bodyElements ++ List(img)
      }
      // link
      else if (sub.startsWith("<div><a href")) {
        var link: String = ""
        while (sub.charAt(0) != '\n') {
          link = link + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        link = link + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        val l: Link = discoverLink(link)
        if (l != null) bodyElements = bodyElements ++ List(l)
      }
      // text elements
      else if (sub.startsWith("<h") || sub.startsWith("<p")) {
        var textEl: String = ""
        textEl = textEl + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
        while (!(sub.startsWith("<img") && sub.startsWith("<a href") ||
          sub.startsWith("<div class=\"col-sm-8 text-left bg-content\">\n<ul>") ||
          sub.startsWith("<div class=\"col-sm-8 text-left bg-content\">\n<ol>") ||
          sub.startsWith("<div class=\"col-sm-10 text-center bg-content\">\n<table class=") ||
          sub.startsWith("<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"")) && sub != end) {
          textEl = textEl + sub.charAt(0)
          sub = sub.replace(sub, sub.substring(1))
        }
        val te: Text = discoverText(textEl)
        if (te != null) bodyElements = bodyElements ++ List(te)
      }
      // unordered list
      else if (sub.startsWith("<div class=\"col-sm-8 text-left bg-content\">\n<ul>")) {
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
        if (unorderedList != null) bodyElements = bodyElements ++ List(unorderedList)
      }
      // ordered list
      else if (sub.startsWith("<div class=\"col-sm-8 text-left bg-content\">\n<ol>")) {
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
        if (orderedList != null) bodyElements = bodyElements ++ List(orderedList)
      }
      // table
      else if (sub.startsWith("<div class=\"col-sm-10 text-center bg-content\">\n<table class=")) {
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
        if (t != null) bodyElements = bodyElements ++ List(t)
      }
      // form
      else if (sub.startsWith("<div class=\"col-sm-8 text-left bg-content container\">\n<form action=")) {
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
        if (f != null) bodyElements = bodyElements ++ List(f)
      } else {
        failure = failure + input + "\n"
        return null
      }
    }
    val body: Body = Body(bodyElements)
    body
  }

  private def discoverHeader(input: String): Header = {
    if (input.substring(0, 67) != "<header>\n<div class=\"jumbotron\">\n<div class=\"container text-left\">\n") {
      failure = failure + input + "\n"
      return null
    }
    var sub: String = input.replace(input, input.substring(67))
    var image: String = ""
    image = image + sub.charAt(0)
    sub = sub.replace(sub, sub.substring(1))
    while (sub.charAt(0) != '<') {
      image = image + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    if (sub.substring(0, 14) != "</div>\n</div>\n") {
      failure = failure + input + "\n"
      return null
    }
    sub = sub.replace(sub, sub.substring(14))
    val img: Image = discoverImage(image)
    var navbar: String = ""
    while (!sub.startsWith("</header>\n")) {
      navbar = navbar + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val nav: Navbar = discoverNavbar(navbar)
    if (img != null && navbar != null) {
      val header: Header = Header(img, nav)
      header
    } else {
      failure = failure + input + "\n"
      null
    }
  }

  private def discoverPage(input: String): Page = {
    if (input.substring(0, 313) != "<!DOCTYPE html>\n<html lang=\"de\">\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">\n<meta charset=\"utf-8\">\n<link rel=\"stylesheet\" href=\"misc/bootstrap.css\">\n<link rel=\"stylesheet\" href=\"misc/my.css\">\n<script src=\"misc/jquery.js\"></script>\n<script src=\"misc/bootstrap.js\"></script>\n</head>\n") {
      failure = failure + input + "\n"
      return null
    }
    if (input.substring(input.length - 7, input.length) != "</html>") {
      failure = failure + input + "\n"
      return null
    }
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
    if (h != null && b != null && f != null) {
      val page: Page = Page(h, b, f)
      page
    } else {
      failure = failure + input + "\n"
      null
    }
  }
}
