package Parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers{
  def identifier: Parser[String] = """(([/\-!:,&a-zA-Z01-9\d\s])+)""".r ^^ {_.toString}
  def word: Parser[String] = """([a-z]+)|([A-Z]+)|(0|[1-9]\d*)""".r ^^ {_.toString}

  def statementWrapWrapWrap = """\(""".r ~ wrapEl ~ (repsep(statementWrapWrap, ",") | repsep(statementWrap, ",")) ~ """\)""".r ^^ {_.toString()}
  def statementWrapWrap = """\(""".r ~  wrapEl ~ repsep(statementWrap, ",") ~ """\)""".r ^^ {_.toString()}

  def wrapEl = (tablerow ~ """:""".r) | (table ~ """:""".r) | (body ~ """:""".r) | (link ~ """:""".r) | (footer ~ """:""".r)^^ {_.toString}

  def statementWrap = """\(""".r ~ wrapEl ~ statement ~ """\)""".r ^^ {_.toString()}

  def statement =  repsep(statementel, ",") ^^ {_.toString()}

  def statementel = destination | ("""\(""".r ~ identifier ~ """\)""".r) |  tabledata | tablehead
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
  def textArea: Parser[String] = """TextArea""".r
  // input = (<label ~ input ~ placeholder>) // Vielleicht müssen die Styling-Tags auch hier mit rein später?! Placeholder ist ja so gesehen ein Teil des Inputs, aber programmiertechnisch ein Styling bzw eine Zusatzspezifikation des Input-Tags
  def input: Parser[String] = """\(""".r ~ label ~ input ~ placeholder ~ """\),""".r ^^ {_.toString()}
  // label = (label)
  def label: Parser[String] = """\(""".r ~ identifier ~ """\),""".r ^^ {_.toString()}
 // placeholder = (placeholder)
  def placeholder: Parser[String] = """Placeholder""".r
  // formular = (<label ~ input ~ placeholder>)
  def formular: Parser[String] = """Input""".r
  // paragraph = (paragraph)
  def paragraph: Parser[String] = """Paragraph""".r
  // headline = (headline) // Hier ggf. mehrere für Unterscheidung h1-h4?
  def headline: Parser[String] = """Headline""".r
  // text = (text)
  def text: Parser[String] = """Text""".r
  // icon = (icon)
  def icon: Parser[String] = """Icon""".r
  // listelement = (listelement)
  def listElement: Parser[String] = """ListElement""".r
  // list = (listelement)*
  def list: Parser[String] = """\(""".r ~ listElement ~ """\*""".r ~ """\)""".r ^^ {_.toString()}
  // image = (image)
  def image: Parser[String] = """Image""".r
  // footer = (footer: <link ~ link>)
  def footer: Parser[String] = """Footer""".r ^^ {_.toString()}
  // navbar = (navbar: <link*>) // ToDo: Überlegen, wie wir umsetzen dass Links auch "Unterlinks" haben können in der Navbar.. zwei Linkarten oder als optionales Attribut hinzufügen oder ganz anders?!
  def navbar: Parser[String] = """\(""".r ~ link ~ """\*""".r ~ """\)""".r ^^ {_.toString()}
  // header = (header: <image ~ navbar>) // CAVE: Tatsächlich ist der header leer, logo befindet sich im Body.. aber sollte ja trotzdem gleich aussehen und irrelevant sein am Ende
  def header: Parser[String] = """\(""".r ~ image ~ navbar ~ """\)""".r ^^ {_.toString()}
  // body = (body: <image* ~ text* ~ list* ~ icon* ~ table* ~ formular* ~ link*>)
  def body: Parser[String] = """Body""".r ^^ {_.toString()}
  // page = (page: <header ~ body ~ footer)
  def page: Parser[String] = """\(""".r ~ header ~ body ~ footer ~ """\)""".r ^^ {_.toString()}
  // website = (website: <page*>)
  def website: Parser[String] = """\(""".r ~ page ~ """\*""".r ~ """\)""".r ^^ {_.toString()}

}
