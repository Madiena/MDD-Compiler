package Discoverer

import Parser.WebsiteParser._

import java.util

class Discoverer() {
  var input: String = ""

  def discover(): Object = {
    // Input
    if (input.contains("<input")) {
      return discoverInput(input)
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
      return disvoverTableRowHead(input)
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
    }
    // ListElement
    else if (input.contains("<li>")) {
      return discoverListElement(input)
    } // Text
    else if (input.contains("<div class=\"col-sm-8 text-left bg-content\">\n<h") || input.contains("<div class=\"col-sm-8 text-left bg-content\">\n<p")) {
      return discoverText(input)
    }
    // Paragraph
    else if (input.contains("<p ")) {
      return discoverParagraph(input)
    } // Headline
    else if (input.contains("<h")) {
      return discoverHeadline(input)
    } // Link
    else if (input.contains("<a href")) {
      return discoverLink(input)
    } // Navbar
    else if (input.contains("<nav class=")) {
      return discoverNavbar(input)
    }
    ""
  }

  def discoverInput(input: String): InputEl = {
    var sub: String = input.replace(input, input.substring(72))
    var id: String = ""
    var placeholder: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
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
    assert(inputEl.toString == "(Input: (Id: (fname)), (Placeholder: (Vorname)))")
    inputEl
  }

  def discoverTextarea(input: String): TextArea = {
    var sub: String = input.replace(input, input.substring(42))
    var id: String = ""
    var placeholder: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
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
    var sub: String = input.replace(input, input.substring(12))
    var id: String = ""
    var in: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
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
      var td: Tabledata = discoverTabledata(tabledata)
      tabledatas = tabledatas ++ List(td)
      tabledata = ""
    }
    val tablerowdata: Tablerowdata = Tablerowdata(tabledatas)
    println(tablerowdata + "\n")
    tablerowdata
  }

  def discoverTabledata(input: String): Tabledata = {
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

  def disvoverTableRowHead(input: String): Tablerowhead = {
    var sub: String = input.replace(input, input.substring(13))
    val end: String = "\n</tr>\n</thead>"
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
      var th: Tablehead = discoverTablehead(tablehead)
      tableheads = tableheads ++ List(th)
      tablehead = ""
    }
    val tablerowhead: Tablerowhead = Tablerowhead(tableheads)
    println(tablerowhead + "\n")
    tablerowhead
  }

  def discoverTable(input: String): Table = {
    var sub: String = input.replace(input, input.substring(69))
    var trhString: String = ""
    val tableEnd: String = "\n</table>\n</div>\n"
    while (sub.contains("</thead>")) {
      trhString = trhString + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    for (i <- 1 to 7) {
      trhString = trhString + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    var tablerowhead: Tablerowhead = disvoverTableRowHead(trhString)
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
    var sub: String = input.replace(input, input.substring(48))
    val end: String = "\n</ol></div>\n"
    var listElement: String = ""
    var list: List[ListElement] = List()
    println(sub)
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
    var sub: String = input.replace(input, input.substring(48))
    val end: String = "\n</ul></div>\n"
    var listElement: String = ""
    var list: List[ListElement] = List()
    println(sub)
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
    var sub: String = input.replace(input, input.substring(2))
    var num: Char = sub.charAt(0)
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
    var sub: String = input.replace(input, input.substring(124))
    var headline: String = ""
    var paragraph: String = ""
    var textEls: List[TextEl] = List()
    val end: String = "</div>\n</div>\n"
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
    }
    val text: Text = Text(textEls)
    println(text + "\n")
    text
  }

  def discoverLink(intput: String): Link = {
    var sub: String = input.replace(input, input.substring(9))
    var destination: String = ""
    var identifier: String = ""
    while (sub.charAt(0) != '"') {
      destination = destination + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
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
    var sub: String = input.replace(input, input.substring(13))
    var destination: String = ""
    var identifier: String = ""
    while (sub.charAt(0) != '"') {
      destination = destination + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
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
    var sub: String = input.replace(input, input.substring(72))
    var id: String = ""
    val end: String = "</ul>\n</li>\n"
    var navlink: String = ""
    var navlinks: List[NavLink] = List()
    while (sub.charAt(0) != '\n') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
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

  def discoverNavbar(input: String): String = {
    var sub: String = input.replace(input, input.substring(135))
    println(sub)
    ""
  }

}
