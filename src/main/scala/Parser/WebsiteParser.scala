package Parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers {
  def website: Parser[Website] =
    """Website: \(""".r ~ page ~ """\)""".r ^^ {
      case s1 ~ p ~ s2 => Website(p);
    }

  private def page: Parser[Page] =
    """Page: \(""".r ~ header ~ """,""" ~ body ~ """,""" ~ footer ~ """\)""".r ^^ {
      case s1 ~ hea ~ s2 ~ bod ~ s3 ~ foo ~ s4 => Page(hea, bod, foo)
    }

  def header: Parser[Header] =
    """\(Header: """.r ~ image ~ """,""".r ~ navbar ~ """\)""".r ^^ {
      case s1 ~ im ~ s2 ~ nb ~ s3 => Header(im, nb) }

  def body: Parser[Body] =
    """\(Body: """.r ~ repsep(bodyEl, ",") ~ """\)""".r ^^ {
      case s1 ~ elList ~ s2 => Body(elList)
    }

  private def bodyEl: Parser[BodyElement] = image | text | unorderedList | orderedList | fullTable | link | form

  def footer: Parser[Footer] =
    """\(Footer:""".r ~ repsep(link, ",") ~ """\)""".r ^^ {
      case s1 ~ liList ~ s2 => Footer(liList)
    }

  private def image: Parser[Image] =
    """\(Image: \(""".r ~ word ~ """[.]jpg\)\)""".r ^^ {
      case s1 ~ id ~ s2 => Image(id)
    }

  private def navbar: Parser[Navbar] =
    """\(Navbar: """.r ~ repsep(navbarEl, ",") ~ """\)""".r ^^ {
      case s1 ~ elList ~ s2 => Navbar(elList)
    }

  private def navbarList: Parser[NavbarList] =
    """\(Dropdown: """.r ~ wrappedIdentifier ~ """,""".r ~ repsep(navLink, ",") ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 ~ liList ~ s3 => NavbarList(id, liList)
    }

  private def navLink: Parser[NavLink] =
    """\(Link:""".r ~ """\(""".r ~ linkId ~ """\),""".r ~ destination ~ """\)""".r ^^ {
      case s1 ~ s2 ~ li ~ s3 ~ des ~ s4 => NavLink(des, li)
    }
  private def navbarEl: Parser[NavbarElement] = navLink | navbarList

  private def linkId: Parser[LinkIdentifier] = identifier ^^ { id => LinkIdentifier(id)}

  private def link: Parser[Link] =
    """\(Link:""".r ~ """\(""".r ~ linkId ~ """\),""".r ~ destination ~ """\)""".r ^^ {
      case s1 ~ s2 ~ li ~ s3 ~ des ~ s4  => Link(des, li)
    }

  private def destination: Parser[Destination] =
    """\(""".r ~ word ~ """\.html""".r ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 ~ s3 => Destination(id)
    }

  def text: Parser[Text] =
    """\(Text: """.r ~ repsep(textEl, ",") ~ """\)""".r ^^ {
      case s1 ~ teList ~ s2 => Text(teList)
    }

  private def textEl: Parser[TextEl] = headline | paragraph

  private def headline: Parser[Headline] =
    """\(Headline """.r ~ hNum ~ """: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {
      case s1 ~ hn ~ s2 ~ id ~ s3 => Headline(id, hn)
    }

  private def paragraph: Parser[Paragraph] =
    """\(Paragraph: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 => Paragraph(id)
    }

  private def unorderedList: Parser[UnorderedList] =
    """\(List unordered:""".r ~ repsep(listElement, ",") ~ """\)""".r ^^ {
      case s1 ~ listElList ~ s2 => UnorderedList(listElList)
    }

  private def orderedList: Parser[OrderedList] =
    """\(List ordered:""".r ~ repsep(listElement, ",") ~ """\)""".r ^^ {case s1 ~ listElList ~ s2 => OrderedList(listElList)
    }

  private def listElement: Parser[ListElement] = wrappedIdentifier ^^ { id => ListElement(id) }

  private def fullTable: Parser[Table] =
    """\(""".r ~ tableWrapEl ~ tableRowHead ~ """,""".r ~ repsep(tableRowData, ",") ~ """\)""".r ^^ {
      case s1 ~ s2 ~ trh ~ s3 ~ trdList ~ s4 => Table(trh, trdList)
    }

  private def tableWrapEl: Parser[String] = """Tablerow:""".r | """Table:""".r

  private def tableRowHead: Parser[Tablerowhead] =
    """\(""".r ~ tableWrapEl ~ repsep(tablehead, ",") ~ """\)""".r ^^ {
      case s1 ~ s2 ~ thList ~ s3 => Tablerowhead(thList)
    }

  private def tableData: Parser[Tabledata] = wrappedIdentifier ^^ { id => Tabledata(id)}

  private def tableRowData: Parser[Tablerowdata] =
    """\(""".r ~ tableWrapEl ~ repsep(tableData, ",") ~ """\)""".r ^^ {
      case s1 ~ s2 ~ tdList ~ s3 => Tablerowdata(tdList)
    }

  private def tablehead: Parser[Tablehead] =
    """\(""".r ~ word ~ """\)""".r ^^ { case s1 ~ id ~ s2 => Tablehead(id)
    }

  def form: Parser[BodyElement] =
    """\(Form: """.r ~ repsep(formEl, ",") ~ """\)""".r ^^ { case s1 ~ feList ~ s2  => Form(feList);}

  private def formEl: Parser[FormEl] = """\(""".r ~ label ~ """,""".r ~ (input | textArea) ~ """\)""".r ^^ {
    case s1 ~ la ~ s2 ~ el ~ s3 => FormEl(la, el);
  }
  private def label: Parser[Label] = """\(Label: """.r ~ formId ~ """,""".r ~ wrappedIdentifier ~ """\)""".r ^^ { case s1 ~ id ~ s2 ~ wi ~ s3 => Label(id, wi) }

  private def input: Parser[InputEl] =
    """\(Input: """.r ~ formId ~ """,""" ~ placeHolder ~ """\)""".r ^^ { case s1 ~ fi ~ s2 ~ ph ~ s3 => InputEl(fi, ph) }

  private def placeHolder: Parser[Placeholder] = """\(Placeholder:""".r ~ wrappedIdentifier ~ """\)""".r ^^ {case s1 ~ id ~ s2 => Placeholder(id) }

  private def formId: Parser[FormIdentifier] = """\(Id:""".r ~ wrappedIdentifier ~ """\)""".r ^^{
    case s1 ~ id ~ s2 => FormIdentifier(id)
  }

  private def textArea: Parser[TextArea] =
    """\(Textarea: """.r ~ formId ~ """,""" ~ placeHolder ~ """\)""".r ^^ { case s1 ~ fi ~ s2 ~ ph ~ s3 => TextArea(fi, ph) }

  private def wrappedIdentifier: Parser[String] =
    """\(""".r ~ identifier ~ """\)""".r ^^ {
      _.toString()
    }

  private def hNum: Parser[Int] = """[1-4]""".r ^^ {_.toInt}
  private def identifier: Parser[String] =
    """(([/\-!.:,'&a-zA-Z01-9\d\s])+)""".r

  def word: Parser[String] =
    """([_a-zA-Z]+)|(0|[1-9]\d*)""".r

  override protected val whiteSpace: Regex = """\s*|//.*""".r


  case class Image(identifier: String) extends BodyElement {
    override def toString: String = "(Image: (" + identifier + "))"
  }

  case class Website(page: Page) {
    override def toString: String = "Website: (" + page + ")"
  }

  case class Page(header: Header, body: Body, footer: Footer) {
    override def toString: String = "Page: " + header.toString() + ", " + body.toString() + ", " + footer.toString() + ")"
  }

  case class Header(image: Image, navbar: Navbar) {
    override def toString: String = "(Header: " + image.toString() + ", " + navbar.toString() + ")"
  }

  case class Body(bodyElements: List[BodyElement]) {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (el <- bodyElements) {
      sb.addString(sb.append(el), ",")
    }
    def toHtml: String = "<body"
    override def toString: String = "(Body: " + sb.toString() + ")"
  }

  sealed abstract class BodyElement() {

  }

  case class Footer(links: List[Link]) {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (link <- links) {
      sb.addString(sb.append(link), ",")
      htmlBuilder.append(link.toHtml)
    }
    def toHtml: String = "<footer class=\"container-fluid text-center\">\n<ul>\n" + htmlBuilder.toString() +
      "</ul>\n</footer>\n"
    override def toString: String = "(Footer: " + sb.toString() + ")"
  }

  case class Navbar(elements: List[NavbarElement]) {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (el <- elements) {
      sb.addString(sb.append(el), ",")
      htmlBuilder.append(el.toHtml)
    }
    def toHtml: String = "<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n" +
      "<ul class=\"nav navbar-nav\">\n" + htmlBuilder.toString() + "</ul>\n</div>\n</div>\n</nav>\n"
    override def toString: String = "(Navbar: " + sb.toString() + ", " + navbarList.toString() + ")"
  }

  sealed abstract class NavbarElement() {
    def toHtml: String
  }

  case class Link(destination: Destination, identifier: LinkIdentifier) extends BodyElement {
    def toHtml: String = "<a href=\"" + destination + "\">" + identifier + "</a>\n"
    override def toString: String = "(Link: (" + identifier + "), (" + destination + "))"
  }

  case class NavLink(destination: Destination, identifier: LinkIdentifier) extends NavbarElement {
    override def toHtml: String = "<li><a href=\"" + destination + "\">" + identifier + "</a></li>\n"
    override def toString: String = "(Link: (" + identifier + "), (" + destination + "))"
  }


  case class Destination(identifier: String) {
    override def toString: String = identifier
  }

  case class LinkIdentifier(identifier: String) {
    override def toString: String = identifier
  }

  case class NavbarList(id: String, links: List[NavLink]) extends NavbarElement {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (link <- links) {
      sb.addString(sb.append(link), ",")
      htmlBuilder.append(link.toHtml)
    }
    override def toHtml: String = "<li class =\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">" + id +
      "\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">" + htmlBuilder.toString() + "\n</ul>\n</li>\n"
    override def toString: String = "(Dropdown: (" + id + "),"  + sb.toString() + ")"
  }

  case class Text(textel: List[TextEl]) extends BodyElement {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (el <- textel) {
      sb.addString(sb.append(el), ",")
      htmlBuilder.append(el.toHtml)
    }
    def toHtml: String = "<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n" + htmlBuilder.toString() + "</div>\n</div>\n"
    override def toString: String = "(Text: " + sb.toString() + ")"
  }

  sealed abstract class TextEl() {
    def toHtml: String
  }

  case class Headline(identifier: String, num: Int) extends TextEl {
    override def toHtml: String = "<h" + num.toString + ">" + identifier + "</h" + num.toString + ">\n"
    override def toString: String = "(Headline: (" + identifier + "))"
  }

  case class Paragraph(identifier: String) extends TextEl {
    override def toHtml: String = "<p style=\"margin-bottom: 25px\">" + identifier + "</p\n>"
    override def toString: String = "(Paragraph: (" + identifier + "))"
  }

  case class UnorderedList(elements: List[ListElement]) extends BodyElement {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (el <- elements) {
      sb.addString(sb.append("(" + el + ")"), ",")
      htmlBuilder.append(el.toHtml)
    }
    def toHtml: String = "<div class=\"col-sm-8 text-left bg-content\">\n<ul>" + htmlBuilder.toString() + "</ul></div>"
    override def toString: String = "(List unordered: " + sb.toString() + ")"
  }

  case class OrderedList(elements: List[ListElement]) extends BodyElement {
    private val sb = new StringBuilder();
    private val htmlBuilder = new StringBuilder()
    for (el <- elements) {
      sb.addString(sb.append("(" + el + ")"), ",")
      htmlBuilder.append(el.toHtml)
    }
    def toHtml: String = "<div class=\"col-sm-8 text-left bg-content\">\n<ol>" + htmlBuilder.toString() + "</ol></div>"
    override def toString: String = "(List ordered: " + sb.toString() + ")"
  }

  case class ListElement(identifier: String) {
    def toHtml: String = "<li>" + identifier + "</li>\n"
    override def toString: String = identifier
  }

  case class Table(tablerowhead: Tablerowhead, tablerowdatas: List[Tablerowdata]) extends BodyElement {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (trd <- tablerowdatas) {
      sb.addString(sb.append(trd), ",")
      htmlBuilder.append(trd.toHtml)
    }
    def toHtml: String = "<div class=\"col-sm-10 text-center bg-content\">\n<table class=\"table\">\n" + tablerowhead.toHtml + "\n" + htmlBuilder.toString() + "</table>\n</div>\n"
    override def toString: String = "(Table: " + tablerowhead + sb.toString() + ")"

  }

  case class Tablerowdata(tabledatas: List[Tabledata]) {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (td <- tabledatas) {
      sb.addString(sb.append("(" + td + ")"), ",")
      htmlBuilder.append(td.toHtml)
    }
    def toHtml: String = "<tr>\n" + htmlBuilder.toString() + "</tr>"
    override def toString: String = "(Tablerow: " + sb.toString() + ")"
  }

  case class Tablerowhead(tableheads: List[Tablehead]) {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (th <- tableheads) {
      sb.addString(sb.append("(" + th + ")"), ",")
      htmlBuilder.append(th.toHtml)
    }
    def toHtml: String = "<thead>\n<tr>\n" + htmlBuilder.toString() + "</tr>\n</thead>"
    override def toString: String = "(Tablerow: " + sb.toString() + ")"
  }

  case class Tablehead(identifier: String) {
    def toHtml: String = "<th class=\"text-center\">" + identifier + "</th>\n"
    override def toString: String = identifier
  }

  case class Tabledata(identifier: String) {
    def toHtml: String = "<td>"+ identifier + "</td>\n"
    override def toString: String = identifier
  }

  private case class Form(formEls: List[FormEl]) extends BodyElement {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (el <- formEls) {
      sb.addString(sb.append("(" + el + ")"), ",")
      htmlBuilder.append(el.toHtml)
    }
    def toHtml: String = "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">" +
      "\n" + htmlBuilder.toString() + "<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>"
    override def toString: String = "(Form: " + sb.toString() + ")"
  }

  case class FormEl(label: Label, formEl: FormElEl) {
    def toHtml: String = label.toHtml + "\n" + formEl.toHtml + "\n"
    override def toString: String = "(" + label.toString() + "," + formEl.toString + ")"
  }

  abstract sealed class FormElEl() {
    def toHtml: String
  }

  case class Label(id: FormIdentifier, identifier: String) {
    def toHtml: String = "<label for=\"" + id + "\">" + identifier + ":</label>"
    override def toString: String = "(Label" + FormIdentifier.toString() + "," + identifier + ")"
  }

  case class TextArea(id: FormIdentifier, placeholder: Placeholder) extends FormElEl {
    override def toHtml: String = "<textarea style=\"margin-bottom: 50px\" id=\"" + id.toString + "\" class=\"form-control\" placeholder=\"" + placeholder.toString + "\" style=\"height:200px\"></textarea>"
    override def toString: String = "(Textarea: (" + placeholder.toString + "))"

  }

  case class Placeholder(identifier: String) {
    override def toString: String = identifier
  }

  case class FormIdentifier(id: String) {
    override def toString: String = id
  }

  case class InputEl(id: FormIdentifier, placeholder: Placeholder) extends FormElEl {
    override def toHtml: String = "<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\""+ id.toString + "\" placeholder=\"" + placeholder.toString + "\">"
    override def toString: String = "(Input: (" + placeholder.toString + "))"

  }
}
