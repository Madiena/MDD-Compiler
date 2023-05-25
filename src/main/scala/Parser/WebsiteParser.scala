package Parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers {
  def website: Parser[String] =
    """Website: \(""".r ~ page ~ """\)""".r ^^ {
      _.toString()
    }

  def page: Parser[String] =
    """Page: \(""".r ~ header ~ """,""" ~ body ~ """,""" ~ footer ~ """\)""".r ^^ {
      _.toString()
    }

  def header: Parser[String] =
    """\(Header: """.r ~ image ~ """,""".r ~ navbar ~ """\)""".r ^^ {
      _.toString()
    }

  def body: Parser[String] =
    """\(Body: """.r ~ repsep(bodyEl, ",") ~ """\)""".r ^^ {
      _.toString()
    }

  def bodyEl = image | text | unorderedList | orderedList | icon | fullTable | link | form ^^ {
    _.toString()
  }

  def footer: Parser[String] =
    """\(Footer:""".r ~ repsep(link, ",") ~ """\)""".r ^^ {
      _.toString()
    }

  def image: Parser[String] =
    """\(Image: \(""".r ~ word ~ """[.]jpg\)\)""".r ^^ {
      _.toString()
    }

  def navbar: Parser[String] =
    """\(Navbar: """.r ~ repsep(navbarEl, ",") ~ """\)""".r ^^ {
      _.toString()
    }

  def navbarList =
    """\(List: """.r ~ repsep(link, ",") ~ """\)""".r ^^ {
      _.toString()
    }

  def navbarEl = link | navbarList ^^ {
    _.toString()
  }

  def link =
    """\(Link:""".r ~ """\(""".r ~ identifier ~ """\),""".r ~ destination ~ """\)""".r ^^ {
      _.toString()
    }

  def destination: Parser[String] =
    """\(""".r ~ word ~ """\.html""".r ~ """\)""".r ^^ {
      _.toString()
    }

  def text: Parser[String] =
    """\(Text: """.r ~ headline ~ paragraph ~ """\)""".r ^^ {
      _.toString()
    }

  def headline: Parser[String] =
    """\(Headline:""".r ~ wrappedIdentifier ~ """\)""".r ^^ {
      _.toString()
    }

  def paragraph: Parser[String] =
    """\(Paragraph: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {
      _.toString()
    }

  def unorderedList: Parser[String] =
    """\(List unordered:""".r ~ repsep(wrappedIdentifier, ",") ~ """\)""".r ^^ {
      _.toString()
    }

  def orderedList: Parser[String] =
    """\(List ordered:""".r ~ repsep(wrappedIdentifier, ",") ~ """\)""".r ^^ {
      _.toString()
    }

  def icon: Parser[String] =
    """\(Icon: \(http://www[.]""".r ~ identifier ~ """\)\)""".r ^^ {
      _.toString()
    }

  def fullTable =
    """\(""".r ~ tableWrapEl ~ tableRowHead ~ """,""".r ~ repsep(tableRowData, ",") ~ """\)""".r ^^ {
      _.toString()
    }

  def tableWrapEl = """Tablerow:""".r | """Table:""".r

  def tableRowHead =
    """\(""".r ~ tableWrapEl ~ repsep(tablehead, ",") ~ """\)""".r ^^ {
      _.toString()
    }

  def tableRowData =
    """\(""".r ~ tableWrapEl ~ repsep(wrappedIdentifier, ",") ~ """\)""".r ^^ {
      _.toString()
    }

  def tablehead: Parser[String] =
    """\(""".r ~ word ~ """\)""".r ^^ {
      _.toString()
    }

  def form =
    """\(Form: """.r ~ repsep(formEl, ",") ^^ {
      _.toString()
    }

  def formEl = wrappedIdentifier ~ """,""".r ~ (input | textArea) ^^ {
    _.toString()
  }

  def input: Parser[String] =
    """\(Input: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {
      _.toString()
    }

  def textArea: Parser[String] =
    """\(Textarea: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {
      _.toString()
    }

  def wrappedIdentifier =
    """\(""".r ~ identifier ~ """\)""".r ^^ {
      _.toString()
    }

  def identifier: Parser[String] =
    """(([/\-!.:,'&a-zA-Z01-9\d\s])+)""".r ^^ {
      _.toString
    }

  def word: Parser[String] =
    """([_a-zA-Z]+)|(0|[1-9]\d*)""".r ^^ {
      _.toString
    }

  override protected val whiteSpace: Regex = """\s*|//.*""".r


  case class Image(identifier: String) extends BodyElement {
    override def toString: String = "(Image: (" + identifier + "))"
  }

  case class Website(page: Page) {
    override def toString = "Website: (" + page + ")"
  }

  case class Page(header: Header, body: Body, footer: Footer) {
    override def toString: String = "Page: " + header.toString() + ", " + body.toString() + ", " + footer.toString() + ")"
  }

  case class Header(image: Image, navbar: Navbar) {
    override def toString: String = "(Header: " + image.toString() + ", " + navbar.toString() + ")"
  }

  case class Body(bodyElements: List[BodyElement]) {
    val sb = new StringBuilder();
    for (el <- bodyElements) {
      sb.addString(sb.append(el), ",")
    }

    override def toString: String = "(Body: " + sb.toString() + ")"
  }

  sealed abstract class BodyElement() {

  }

  case class Footer(links: List[Link]) {
    val sb = new StringBuilder();
    for (link <- links) {
      sb.addString(sb.append(link), ",")
    }

    override def toString: String = "(Footer: " + sb.toString() + ")"
  }

  case class Navbar(elements: List[NavbarElement]) {
    val sb = new StringBuilder();
    for (el <- elements) {
      sb.addString(sb.append(el), ",")
    }

    override def toString: String = "(Navbar: " + sb.toString() + ", " + navbarList.toString() + ")"
  }

  sealed abstract class NavbarElement() {

  }

  case class Link(destination: Destination, identifier: LinkIdentifier) extends BodyElement {
    override def toString: String = "(Link: (" + identifier + "), (" + destination + "))"
  }

  case class NavLink(destination: Destination, identifier: LinkIdentifier) extends NavbarElement {
    override def toString: String = "(Link: (" + identifier + "), (" + destination + "))"
  }


  case class Destination(identifier: String) {
    override def toString: String = identifier
  }

  case class LinkIdentifier(identifier: String) {
    override def toString: String = identifier
  }

  case class NavbarList(links: List[Link]) extends NavbarElement {
    val sb = new StringBuilder();
    for (link <- links) {
      sb.addString(sb.append(link), ",")
    }

    override def toString: String = "(List unordered: " + sb.toString() + ")"
  }

  case class Text(headline: Headline, paragraph: Paragraph) extends BodyElement {
    override def toString: String = "(Text: " + headline + "," + paragraph + ")"
  }

  case class Headline(identifier: String) {
    override def toString: String = "(Headline: (" + identifier + "))"
  }

  case class Paragraph(identifier: String) {
    override def toString: String = "(Paragraph: (" + identifier + "))"
  }

  case class UnorderedList(elements: List[ListElement]) extends BodyElement {
    val sb = new StringBuilder();
    for (el <- elements) {
      sb.addString(sb.append("(" + el + ")"), ",")
    }

    override def toString: String = "(List unordered: " + sb.toString() + ")"
  }

  case class OrderedList(elements: List[ListElement]) extends BodyElement {
    val sb = new StringBuilder();
    for (el <- elements) {
      sb.addString(sb.append("(" + el + ")"), ",")
    }

    override def toString: String = "(List ordered: " + sb.toString() + ")"
  }

  case class ListElement(identifier: String) {
    override def toString: String = identifier
  }

  case class Icon(identifier: String) extends BodyElement {
    override def toString: String = identifier
  }

  case class Table(tablerowheads: List[Tablerowhead], tablerowdatas: List[Tablerowdata]) extends BodyElement {
    var sb = new StringBuilder()
    for (trh <- tablerowheads) {
      sb.addString(sb.append(trh), ",")
    }
    val str: String = sb.toString()
    sb = new StringBuilder()
    for (trd <- tablerowdatas) {
      sb.addString(sb.append(trd), ",")
    }

    override def toString: String = "(Table: " + str + sb.toString() + ")"

  }

  case class Tablerowdata(tabledatas: List[Tabledata]) {
    val sb = new StringBuilder();
    for (td <- tabledatas) {
      sb.addString(sb.append("(" + td + ")"), ",")
    }

    override def toString: String = "(Tablerow: " + sb.toString() + ")"
  }

  case class Tablerowhead(tableheads: List[Tablehead]) {
    val sb = new StringBuilder();
    for (th <- tableheads) {
      sb.addString(sb.append("(" + th + ")"), ",")
    }

    override def toString: String = "(Tablerow: " + sb.toString() + ")"
  }

  case class Tablehead(identifier: String) {
    override def toString: String = identifier
  }

  case class Tabledata(identifier: String) {
    override def toString: String = identifier
  }

  case class Form(textArea: TextArea) extends BodyElement {
    override def toString: String = "(Form: " + textArea.toString + ")"
  }

  case class TextArea(placeholder: Placeholder) {
    override def toString: String = "(Textarea: (" + placeholder.toString + "))"
  }

  case class Placeholder(identifier: String) {
    override def toString: String = identifier
  }

  case class InputEl(placeholder: Placeholder) {
    override def toString: String = "(Input: (" + placeholder.toString + "))"
  }
}
