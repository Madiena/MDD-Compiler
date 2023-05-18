package Parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers{
  def bodyEl = image | text | unorderedList | orderedList | icon | fullTable | fullLink | fullForm ^^ {_.toString()}
  def identifier: Parser[String] = """(([/\-!.:,'&a-zA-Z01-9\d\s])+)""".r ^^ {_.toString}
  def word: Parser[String] = """([_a-zA-Z]+)|(0|[1-9]\d*)""".r ^^ {_.toString}
  def fullTable = """\(""".r ~  tableWrapEl ~ tableRowHead ~ """,""".r ~ repsep(tableRowData, ",") ~ """\)""".r ^^ {_.toString()}
  def tableRowHead = """\(""".r ~ tableWrapEl ~ repsep(tablehead, ",") ~ """\)""".r ^^ {_.toString()}
  def tableRowData = """\(""".r ~ tableWrapEl ~ repsep(tabledata, ",") ~ """\)""".r ^^ {_.toString()}
  def tableWrapEl = (tablerow ~ """:""".r) | (table ~ """:""".r)
  def fullLink = """\(""".r ~ link ~ """:""".r ~ """\(""".r ~ identifier ~ """\),""".r ~ destination ~  """\)""".r ^^ {_.toString()}
  def fullForm = """\(""".r ~ form ~ """:""".r ~ repsep(formEl, ",") ^^ {_.toString()}
  def formEl = label ~ """,""".r ~ (input | textArea) ^^ {_.toString()}
  //def fullText;
  def textContent = """\(""".r ~ identifier ~ """\)""".r ^^ {_.toString()}
  // Identifier = (identifier),
  override protected val whiteSpace: Regex = """\s*|//.*""".r
  // Destination = (destination)
  def destination: Parser[String] = """\(""".r ~ word ~ """\.html""".r ~ """\)""".r ^^ {_.toString()}
  // Tablehead = (tablehead),
  def tablehead: Parser[String] = """\(""".r ~ word ~ """\)""".r ^^ {_.toString()}
  // Tabledata = (tabledata),
  def tabledata: Parser[String] = """\(""".r ~ identifier ~ """\)""".r ^^ {_.toString()}
  // Tablerowhead = (tablerow: <tablehead+>),
  def tablerow: Parser[String] =  """Tablerow""".r

  def table: Parser[String] =  """Table""".r

  // Link = (link: <identifier ~ destination>)
  def link: Parser[String] = """Link""".r
  // textArea = (textarea)
  def textArea: Parser[String] = """\(Textarea: """.r ~ placeholder ~ """\)""".r ^^ {_.toString()}
  // input = (<label ~ input ~ placeholder>) // Vielleicht müssen die Styling-Tags auch hier mit rein später?! Placeholder ist ja so gesehen ein Teil des Inputs, aber programmiertechnisch ein Styling bzw eine Zusatzspezifikation des Input-Tags
  def input: Parser[String] = """\(Input: """.r ~ placeholder ~ """\)""".r ^^ {_.toString()}
  // label = (label)
  def label: Parser[String] = """\(""".r ~ identifier ~ """\)""".r ^^ {_.toString()}
 // placeholder = (placeholder)
  def placeholder: Parser[String] = """\(""".r ~ identifier ~ """\)""".r ^^ {_.toString()}
  // formular = (<label ~ input ~ placeholder>)
  def form: Parser[String] = """Form""".r
  // paragraph = (paragraph)
  def paragraph: Parser[String] = """\(Paragraph: """.r ~ textContent ~ """\)""".r ^^ {_.toString()}
  // headline = (headline) // Hier ggf. mehrere für Unterscheidung h1-h4?
  def headline: Parser[String] = """\(Headline:""".r ~ textContent ~ """\)""".r ^^ {_.toString()}
  def textEl = paragraph | headline
  // text = (text)
  def text: Parser[String] = """\(Text: """.r ~ repsep(textEl, ",") ~ """\)""".r ^^ {_.toString()}
  // icon = (icon)
  def icon: Parser[String] = """\(Icon: \(http://www[.]""".r ~ identifier ~ """\)\)""".r ^^ {_.toString()}
  // listelement = (listelement)
  def listElement: Parser[String] = """\(""".r ~ identifier ~ """\)""".r ^^ {_.toString()}
  // list = (listelement)*
  def unorderedList: Parser[String] = """\(List unordered:""".r ~ repsep(listElement, ",") ~ """\)""".r ^^ {_.toString()}
  def orderedList: Parser[String] = """\(List ordered:""".r ~ repsep(listElement, ",") ~ """\)""".r ^^ {_.toString()}
  def navbarList = """\(List: """.r ~ repsep(fullLink, ",") ~ """\)""".r ^^ {_.toString()}
  // image = (image)
  def image: Parser[String] = """\(Image: \(""".r ~ word ~ """[.]jpg\)\)""".r ^^ {_.toString()}
  // footer = (footer: <link ~ link>)
  def footer: Parser[String] = """Footer""".r ^^ {_.toString()}
  def navbar: Parser[String] = """\(Navbar: """.r ~ repsep(navbarEl, ",") ~ """\)""".r ^^ {_.toString()}
  def navbarEl = fullLink | navbarList
  // header = (header: <image ~ navbar>) // CAVE: Tatsächlich ist der header leer, logo befindet sich im Body.. aber sollte ja trotzdem gleich aussehen und irrelevant sein am Ende
  def header: Parser[String] = """Header: """.r ~ image ~ """,""".r ~ navbar ^^ {_.toString()}
  // body = (body: <image* ~ text* ~ list* ~ icon* ~ table* ~ formular* ~ link*>)
  def body: Parser[String] = """Body: """.r ~ repsep(bodyEl, ",") ^^ {_.toString()}
  // page = (page: <header ~ body ~ footer)
  def page: Parser[String] = """Page: \(""".r ~ header ~ """,""" ~ body ~ """,""" ~ footer ~ """\)""".r ^^ {_.toString()}
  // website = (website: <page*>)
  def website: Parser[String] = """Website: \(""".r ~ page ~ """\)""".r ^^ {_.toString()}

}
