package Parser

import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers{

  def word: Parser[String] = """([a-z]+)|([A-Z]+)|(0|[1-9]\d*)""".r ^^ {_.toString}
  def identifierWord: Parser[String] = """([ /\-!:,&)]*)""".r | word ^^ {_.toString}

  def statmentmiddleel = """\(""".r ~ id ~ """\),""".r

  def statementendel = """\(""".r ~ id ~ """\)""".r
  def statementlist = id ~ """:""".r ~ rep1(statementendel) ~ statementendel

  def endel = destination | tablehead | tabledata | tablerow |

  // Destination = (destination)
  def destination: Parser[String] = """\(""".r ~ word ~ """\((.html)\),""".r ~ """\)""".r ^^ {_.toString()}
  // Tablehead = (tablehead),
  def tablehead: Parser[String] = """\(""".r ~ word ~ """\),""".r ^^ {_.toString()}
  // Tableheadend = (tablehead)
  def tableheadend: Parser[String] = """\(""".r ~ word ~ """\)""".r ^^ {_.toString()}
  // Tabledata = (tabledata),
  def tabledata: Parser[String] = """\(""".r ~ identifierWord ~ """\),""".r ^^ {_.toString()}
  // Tabledataend = (tabledata)
  def tabledataend = """\(""".r ~ identifierWord ~ """\)""".r ^^ {_.toString()}
  // Tablerowhead = (tablerow: <tablehead+>),
  def tablerowhead  = """\(Tablerow:""".r ~ (rep1(tablehead) ~ tableheadend) ~ """\),""".r  //TODO: function building AST node
  // Tablerowheadend = (tablerow: <tablehead+>)
  def tablerowheadend  = """\(Tablerow:""".r ~ (rep1(tablehead) ~ tableheadend) ~ """\)""".r  //TODO: function building AST node
  // Tablerowdata = (tablerow: <tabledata+>),
  def tablerowdata  = """\(Tablerow:""".r ~ (rep1(tabledata) ~ tabledataend) ~ """\),""".r  //TODO: function building AST node
  // Tablerowdataend = (tablerow: <tabledata+>)
  def tablerowdataend  = """\(Tablerow:""".r ~ (rep1(tabledata) ~ tabledataend) ~ """\)""".r  //TODO: function building AST node
  // Table = (table: <tablerow+>)
  def table = """\(Table:""".r  ~ rep1(tablerowhead) ~ tablerowheadend ~ rep1(tablerowdata) ~ tablerowdataend ~ """\),""".r //TODO: function building AST node
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
