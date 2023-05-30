package Parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers {
  def website: Parser[Website] =
    """Website: \(""".r ~ page ~ """\)""".r ^^ {
      case s1 ~ p ~ s2 => Website(p);
    }

  def page: Parser[Page] =
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

  def bodyEl: Parser[BodyElement] = image | text | unorderedList | orderedList | icon | fullTable | link | form

  def footer: Parser[Footer] =
    """\(Footer:""".r ~ repsep(link, ",") ~ """\)""".r ^^ {
      case s1 ~ liList ~ s2 => Footer(liList)
    }

  def image: Parser[Image] =
    """\(Image: \(""".r ~ word ~ """[.]jpg\)\)""".r ^^ {
      case s1 ~ id ~ s2 => Image(id)
    }

  def navbar: Parser[Navbar] =
    """\(Navbar: """.r ~ repsep(navbarEl, ",") ~ """\)""".r ^^ {
      case s1 ~ elList ~ s2 => Navbar(elList)
    }

  def navbarList: Parser[NavbarList] =
    """\(List: """.r ~ repsep(navLink, ",") ~ """\)""".r ^^ {
      case s1 ~ liList ~ s2 => NavbarList(liList)
    }

  def navLink: Parser[NavLink] =
    """\(Link:""".r ~ """\(""".r ~ linkId ~ """\),""".r ~ destination ~ """\)""".r ^^ {
      case s1 ~ s2 ~ li ~ s3 ~ des ~ s4 => NavLink(des, li)
    }
  def navbarEl: Parser[NavbarElement] = navLink | navbarList

  def linkId: Parser[LinkIdentifier] = identifier ^^ {id => LinkIdentifier(id)}

  def link: Parser[Link] =
    """\(Link:""".r ~ """\(""".r ~ linkId ~ """\),""".r ~ destination ~ """\)""".r ^^ {
      case s1 ~ s2 ~ li ~ s3 ~ des ~ s4  => Link(des, li)
    }

  def destination: Parser[Destination] =
    """\(""".r ~ word ~ """\.html""".r ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 ~ s3 => Destination(id)
    }

  def text: Parser[Text] =
    """\(Text: """.r ~ repsep(textEl, ",") ~ """\)""".r ^^ {
      case s1 ~ teList ~ s2 => Text(teList)
    }

  def textEl: Parser[TextEl] = headline | paragraph

  def headline: Parser[Headline] =
    """\(Headline:""".r ~ wrappedIdentifier ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 => Headline(id)
    }

  def paragraph: Parser[Paragraph] =
    """\(Paragraph: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 => Paragraph(id)
    }

  def unorderedList: Parser[UnorderedList] =
    """\(List unordered:""".r ~ repsep(listElement, ",") ~ """\)""".r ^^ {
      case s1 ~ listElList ~ s2 => UnorderedList(listElList)
    }

  def orderedList: Parser[OrderedList] =
    """\(List ordered:""".r ~ repsep(listElement, ",") ~ """\)""".r ^^ {case s1 ~ listElList ~ s2 => OrderedList(listElList)
    }

  def listElement: Parser[ListElement] = wrappedIdentifier ^^ { id => ListElement(id) }

  def icon: Parser[Icon] =
    """\(Icon: \(http://www[.]""".r ~ identifier ~ """\)\)""".r ^^ {
      case s1 ~ id ~ s3 => Icon(id)
    }

  def fullTable: Parser[Table] =
    """\(""".r ~ tableWrapEl ~ tableRowHead ~ """,""".r ~ repsep(tableRowData, ",") ~ """\)""".r ^^ {
      case s1 ~ s2 ~ trh ~ s3 ~ trdList ~ s4 => Table(trh, trdList)
    }

  def tableWrapEl: Parser[String] = """Tablerow:""".r | """Table:""".r

  def tableRowHead: Parser[Tablerowhead] =
    """\(""".r ~ tableWrapEl ~ repsep(tablehead, ",") ~ """\)""".r ^^ {
      case s1 ~ s2 ~ thList ~ s3 => Tablerowhead(thList)
    }

  def tableData: Parser[Tabledata] = wrappedIdentifier ^^ {id => Tabledata(id)}

  def tableRowData: Parser[Tablerowdata] =
    """\(""".r ~ tableWrapEl ~ repsep(tableData, ",") ~ """\)""".r ^^ {
      case s1 ~ s2 ~ tdList ~ s3 => Tablerowdata(tdList)
    }

  def tablehead: Parser[Tablehead] =
    """\(""".r ~ word ~ """\)""".r ^^ { case s1 ~ id ~ s2 => Tablehead(id)
    }

  def form: Parser[BodyElement] =
    """\(Form: """.r ~ repsep(formEl, ",") ~ """\)""".r ^^ { case s1 ~ feList ~ s2 => Form(feList)}

  def formEl: Parser[FormEl] = """\(""".r ~ label ~ """,""".r ~ (input | textArea) ~ """\)""".r ^^ { case s1 ~ la ~ s2 ~ el ~ s3=> FormEl(la, el)
  }
  def label: Parser[Label] = wrappedIdentifier ^^ { id => Label(id) }

  def input: Parser[InputEl] =
    """\(Input: """.r ~ placeHolder ~ """\)""".r ^^ { case s1 ~ ph ~ s2 => InputEl(ph) }

  def placeHolder: Parser[Placeholder] = wrappedIdentifier ^^ { id => Placeholder(id) }

  def textArea: Parser[TextArea] =
    """\(Textarea: """.r ~ placeHolder ~ """\)""".r ^^ { case s1 ~ ph ~ s2 => TextArea(ph) }

  def wrappedIdentifier: Parser[String] =
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
    override def toString: String = "Website: (" + page + ")"
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

  case class NavbarList(links: List[NavLink]) extends NavbarElement {
    val sb = new StringBuilder();
    for (link <- links) {
      sb.addString(sb.append(link), ",")
    }

    override def toString: String = "(List unordered: " + sb.toString() + ")"
  }

  case class Text(textel: List[TextEl]) extends BodyElement {
    val sb = new StringBuilder();
    for (el <- textel) {
      sb.addString(sb.append(link), ",")
    }
    override def toString: String = "(Text: " + sb.toString() + ")"
  }

  sealed abstract class TextEl() {

  }

  case class Headline(identifier: String) extends TextEl {
    override def toString: String = "(Headline: (" + identifier + "))"
  }

  case class Paragraph(identifier: String) extends TextEl {
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

  case class Table(tablerowhead: Tablerowhead, tablerowdatas: List[Tablerowdata]) extends BodyElement {
    val sb = new StringBuilder()
    for (trd <- tablerowdatas) {
      sb.addString(sb.append(trd), ",")
    }

    override def toString: String = "(Table: " + tablerowhead + sb.toString() + ")"

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

  case class Form(formEls: List[FormEl]) extends BodyElement {
    val sb = new StringBuilder();
    for (el <- formEls) {
      sb.addString(sb.append("(" + el + ")"), ",")
    }
    override def toString: String = "(Form: " + sb.toString() + ")"
  }

  case class FormEl(label: Label, formEl: FormElEl) {
    override def toString: String = "(" + label.toString() + "," + formEl.toString + ")"
  }

  abstract sealed class FormElEl() {

  }

  case class Label(identifier: String) {
    override def toString: String = "(" + identifier + ")"
  }

  case class TextArea(placeholder: Placeholder) extends FormElEl {
    override def toString: String = "(Textarea: (" + placeholder.toString + "))"
  }

  case class Placeholder(identifier: String) {
    override def toString: String = identifier
  }

  case class InputEl(placeholder: Placeholder) extends FormElEl {
    override def toString: String = "(Input: (" + placeholder.toString + "))"
  }
}
