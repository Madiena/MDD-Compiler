package Parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers{
  def identifier: Parser[String] = """(([/\-!:,&a-zA-Z01-9\d\s])+)""".r ^^ {_.toString}
  def word: Parser[String] = """([a-z]+)|([A-Z]+)|(0|[1-9]\d*)""".r ^^ {_.toString}

 // def statmentmiddleel = """\(""".r ~ id ~ """\),""".r
//TODO: find out how to escape ( and ) in Scala regex
  // https://stackoverflow.com/questions/24564872/how-do-i-match-the-contents-of-parenthesis-in-a-scala-regular-expression
  def statementendel: Parser[String] = """\(""".r ~ endel ~ """\)""".r ^^ {_.toString()}

  def statementmiddleel = """\(""".r ~ middleel ~ """\),""".r ^^ {_.toString()}

  // Identifier = (identifier),

  override protected val whiteSpace: Regex = """\s*|//.*""".r

  // def statementlist = id ~ """:""".r ~ rep1(statementendel) ~ statementendel

  def middleel = identifier | tabledata | tablehead

  def endel = destination  | tabledata | tablehead
  // Destination = (destination)
  def destination: Parser[String] =  word ~ """\.html""".r ^^ {_.toString()}
  // Tablehead = (tablehead),
  def tablehead: Parser[String] = word ^^ {_.toString()}
  // Tabledata = (tabledata),
  def tabledata: Parser[String] = identifier ^^ {_.toString()}
  // Tablerowhead = (tablerow: <tablehead+>),
  def tablerowhead  = """\(Tablerow:""".r ~ (rep1(tablehead)) ~ """\),""".r  //TODO: function building AST node
  // Tablerowheadend = (tablerow: <tablehead+>)
  def tablerowheadend  = """\(Tablerow:""".r ~ (rep1(tablehead)) ~ """\)""".r  //TODO: function building AST node
  // Tablerowdata = (tablerow: <tabledata+>),
  def tablerowdata  = """\(Tablerow:""".r ~ (rep1(tabledata) ~ tabledataend) ~ """\),""".r  //TODO: function building AST node
  // Tablerowdataend = (tablerow: <tabledata+>)
  def tablerowdataend  = """\(Tablerow:""".r ~ (rep1(tabledata) ~ tabledataend) ~ """\)""".r  //TODO: function building AST node
  // Table = (table: <tablerow+>)
  def table = """\(Table:""".r  ~ rep1(tablerowhead) ~ tablerowheadend ~ rep1(tablerowdata) ~ tablerowdataend ~ """\),""".r //TODO: function building AST node

  // Link = (link: <identifier ~ destination>)
 /* def link:
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
*/

}
