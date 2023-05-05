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
  // Link = (link: <identifer ~ destination>
  def link:
  def textArea:
  def input:
  def label:
  def formular:
  def paragraph:
  def headline:
  def text:
  def icon:
  def listElement:
  def list:
  def image:
  def footer:
  def navbar:
  def image:
  def header:
  def body:
  def page:
  def website:


}
