package Parser
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers{
  def website: Parser[String] = """Website: \(""".r ~ page ~ """\)""".r ^^ {_.toString()}
  def page: Parser[String] = """Page: \(""".r ~ header ~ """,""" ~ body ~ """,""" ~ footer ~ """\)""".r ^^ {_.toString()}
  def header: Parser[String] = """\(Header: """.r ~ image ~ """,""".r ~ navbar ~ """\)""".r ^^ {_.toString()}
  def body: Parser[String] = """\(Body: """.r ~ repsep(bodyEl, ",") ~ """\)""".r ^^ {_.toString()}
  def bodyEl = image | text | unorderedList | orderedList | icon | fullTable | link | form ^^ {_.toString()}
  def footer: Parser[String] = """\(Footer:""".r ~ repsep(link, ",") ~  """\)""".r ^^ {_.toString()}
  def image: Parser[String] = """\(Image: \(""".r ~ word ~ """[.]jpg\)\)""".r ^^ {_.toString()}
  def navbar: Parser[String] = """\(Navbar: """.r ~ repsep(navbarEl, ",") ~ """\)""".r ^^ {_.toString()}
  def navbarList = """\(List: """.r ~ repsep(link, ",") ~ """\)""".r ^^ {_.toString()}
  def navbarEl = link | navbarList ^^ {_.toString()}
  def link = """\(Link:""".r ~ """\(""".r ~ identifier ~ """\),""".r ~ destination ~  """\)""".r ^^ {_.toString()}
  def destination: Parser[String] = """\(""".r ~ word ~ """\.html""".r ~ """\)""".r ^^ {_.toString()}
  def text: Parser[String] = """\(Text: """.r ~ repsep(textEl, ",") ~ """\)""".r ^^ {_.toString()}
  def textEl = paragraph | headline
  def headline: Parser[String] = """\(Headline:""".r ~ wrappedIdentifier ~ """\)""".r ^^ {_.toString()}
  def paragraph: Parser[String] = """\(Paragraph: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {_.toString()}
  def unorderedList: Parser[String] = """\(List unordered:""".r ~ repsep(wrappedIdentifier, ",") ~ """\)""".r ^^ {_.toString()}
  def orderedList: Parser[String] = """\(List ordered:""".r ~ repsep(wrappedIdentifier, ",") ~ """\)""".r ^^ {_.toString()}
  def icon: Parser[String] = """\(Icon: \(http://www[.]""".r ~ identifier ~ """\)\)""".r ^^ {_.toString()}
  def fullTable = """\(""".r ~  tableWrapEl ~ tableRowHead ~ """,""".r ~ repsep(tableRowData, ",") ~ """\)""".r ^^ {_.toString()}
  def tableWrapEl = """Tablerow:""".r | """Table:""".r
  def tableRowHead = """\(""".r ~ tableWrapEl ~ repsep(tablehead, ",") ~ """\)""".r ^^ {_.toString()}
  def tableRowData = """\(""".r ~ tableWrapEl ~ repsep(wrappedIdentifier, ",") ~ """\)""".r ^^ {_.toString()}
  def tablehead: Parser[String] = """\(""".r ~ word ~ """\)""".r ^^ {_.toString()}
  def form = """\(Form: """.r ~ repsep(formEl, ",") ^^ {_.toString()}
  def formEl = wrappedIdentifier ~ """,""".r ~ (input | textArea) ^^ {_.toString()}
  def input: Parser[String] = """\(Input: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {_.toString()}
  def textArea: Parser[String] = """\(Textarea: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {_.toString()}
  def wrappedIdentifier = """\(""".r ~ identifier ~ """\)""".r ^^ {_.toString()}
  def identifier: Parser[String] = """(([/\-!.:,'&a-zA-Z01-9\d\s])+)""".r ^^ {_.toString}
  def word: Parser[String] = """([_a-zA-Z]+)|(0|[1-9]\d*)""".r ^^ {_.toString}
  override protected val whiteSpace: Regex = """\s*|//.*""".r


  case class Website() {
  }
  case class Page() {
  }
  case class Header() {
  }
  case class Body() {
  }
  case class Footer() {
  }
  case class Image() {
  }
  case class Navbar() {
  }
  case class Link() {
  }
  case class Destination() {
  }
  case class Identifier() {
  }
  case class NavbarList() {
  }
  case class Text() {
  }
  case class Headline() {
  }
  case class Paragraph() {
  }
  case class UnorderedList() {
  }
  case class OrderedList() {
  }
  case class ListElement() {
  }
  case class Icon() {
  }
}
