package Parser

import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers{

  def word: Parser[String] = """([a-z]+)|([A-Z]+)|(0|[1-9]\d*)""".r ^^ {_.toString}
  def identifierWord: Parser[String] = """([ /\-!:,&)]*)""".r | word ^^ {_.toString}

  // Destination = (destination)
  def destination: Parser[String] = """\(""".r ~ word ~ """\((.html)\),""".r ~ """\)""".r ^^ {_.toString()}
  // Tablehead = (tablehead),
  def tablehead: Parser[String] = """\(""".r ~ word ~ """\),""".r ^^ {_.toString()}
  // Tableheadend = (tablehead)
  def tableheadend: Parser[String] = """\(""".r ~ word ~ """\)""".r ^^ {_.toString()}
  // Tabledata = (tabledata),
  def tabledata: Parser[String] = """\(""".r ~ identifierWord ~ """\),""".r ^^ {_.toString()}
  // TODO: tabledataend, like tabledata but w/ trailing comma
  // Tablerow = (tablerow: <tabledata+ | tablehead+>), TODO: Check whether tabledata and tablehead can be mixed or not
  def tablerow  = """\(Tablerow:""".r ~ (rep1(tablehead) | rep1(tabledata)) ~ """\),""".r  //TODO: function building AST node
  // Table = (table: <tablerow+>)
  def table = """\(Table:""".r  ~ rep1(tablerow) ~ """\),""".r //TODO: function building AST node
  // Identifier = (identifier),
  def identifier = """\(""".r ~ identifierWord ~ """\),""".r ^^ {_.toString()}
  // Link = (link: <identifier ~ destination>)
  def link:
  // textArea = (textarea),
  def textArea:
  // input = (<label ~ input ~ placeholder>) // Vielleicht müssen die Styling-Tags auch hier mit rein später?! Placeholder ist ja so gesehen ein Teil des Inputs, aber programmiertechnisch ein Styling bzw eine Zusatzspezifikation des Input-Tags
  def input:
  // label = (label)
  def label: Parser[String] = """\(""".r ~ identifierWord ~ """\),""".r ^^ {_.toString()}
  // formular = (<label ~ input ~ placeholder>)
  def formular:
  // paragraph = (paragraph)
  def paragraph:
  // headline = (headline) // Hier ggf. mehrere für Unterscheidung h1-h4?
  def headline:
  // text = (text)
  def text:
  // icon = (icon)
  def icon:
  // listelement = (listelement)
  def listElement:
  // list = (listelement)*
  def list:
  // image = (image)
  def image:
  // footer = (footer: <link ~ link>)
  def footer:
  // navbar = (navbar: <link*>) // ToDo: Überlegen, wie wir umsetzen dass Links auch "Unterlinks" haben können in der Navbar.. zwei Linkarten oder als optionales Attribut hinzufügen oder ganz anders?!
  def navbar:
  // header = (header: <image ~ navbar>) // CAVE: Tatsächlich ist der header leer, logo befindet sich im Body.. aber sollte ja trotzdem gleich aussehen und irrelevant sein am Ende
  def header:
  // body = (body: <image* ~ text* ~ list* ~ icon* ~ table* ~ formular* ~ link*>)
  def body:
  // page = (page: <header ~ body ~ footer)
  def page:
  // website = (website: <page*>)
  def website:


}
