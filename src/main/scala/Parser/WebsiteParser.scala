package Parser

import scala.util.parsing.combinator.RegexParsers

class WebsiteParser extends RegexParsers{

  def word: Parser[String] = """([a-z]+)|([A-Z]+)|(0|[1-9]\d*)""".r ^^ {_.toString}
  def identifier: Parser[String] = """( |/|-|!|:|,|&)""".r | word ^^ {_.toString}
  def destination: Parser[String] = word ~ """(.html)""".r ^^ {_.toString()}
  def tablehead: Parser[String] = word ^^ {_.toString()}
  def tabledata: Parser[String] = identifier ^^ {_.toString()}

  def tablerowlist: Parser[] = tablerowlist ~ tablerow
  def tablerow: Parser[String]  = tablehead | tabledata
  def table:
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
