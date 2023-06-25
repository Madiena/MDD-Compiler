package Parser

import scala.util.parsing.combinator._
import Utils.Writer

import java.io.File
import scala.language.postfixOps
import scala.sys.exit
import scala.util.matching.Regex

class WebsiteParser extends RegexParsers {

  import WebsiteParser._

  def website: Parser[Website] =
    """Website:""" ~ repsep(page, ",") ^^ {
      case s1 ~ p => Website(p);
    }

  def page: Parser[Page] =
    """\(Page:""".r ~ header ~ """,""" ~ body ~ """,""" ~ footer ~ """\)""".r ^^ {
      case s1 ~ hea ~ s2 ~ bod ~ s3 ~ foo ~ s4 => Page(hea, bod, foo)
    }

  def header: Parser[Header] =
    """\(Header:""".r ~ image ~ """,""".r ~ navbar ~ """\)""".r ^^ {
      case s1 ~ im ~ s2 ~ nb ~ s3 => Header(im, nb)
    }

  def body: Parser[Body] =
    """\(Body:""".r ~ repsep(bodyEl, ",") ~ """\)""".r ^^ {
      case s1 ~ elList ~ s2 => Body(elList)
    }

  private def bodyEl: Parser[BodyElement] = image | text | unorderedList | orderedList | fullTable | link | form

  def footer: Parser[Footer] =
    """\(Footer:""".r ~ repsep(link, ",") ~ """\)""".r ^^ {
      case s1 ~ liList ~ s2 => Footer(liList)
    }

  def image: Parser[Image] =
    """\(Image:\(""".r ~ imageIdentifier ~ ("""[.]jpg""".r | """[.]png""".r) ~ """\)\)""".r ^^ {
      case s1 ~ id ~ s2 ~ s3 => Image(id + s2)
    }

  def navbar: Parser[Navbar] =
    """\(Navbar:""".r ~ repsep(navbarEl, ",") ~ """\)""".r ^^ {
      case s1 ~ elList ~ s2 => Navbar(elList)
    }

  def navbarList: Parser[NavbarList] =
    """\(Dropdown:""".r ~ wrappedIdentifier ~ """,""".r ~ repsep(navLink, ",") ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 ~ liList ~ s3 => NavbarList(id, liList)
    }

  def navLink: Parser[NavLink] =
    """\(Link:""".r ~ """\(""".r ~ linkId ~ """\),""".r ~ destination ~ """\)""".r ^^ {
      case s1 ~ s2 ~ li ~ s3 ~ des ~ s4 => NavLink(des, li)
    }

  private def navbarEl: Parser[NavbarElement] = navbarList | navLink

  private def linkId: Parser[LinkIdentifier] = identifier ^^ { id => LinkIdentifier(id) }

  def link: Parser[Link] =
    """\(Link:""".r ~ """\(""".r ~ linkId ~ """\),""".r ~ destination ~ """\)""".r ^^ {
      case s1 ~ s2 ~ li ~ s3 ~ des ~ s4 => Link(des, li)
    }

  def destination: Parser[Destination] =
    """\(""".r ~ word ~ """\.html""".r ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 ~ s3 => Destination(id + s2)
    }

  def text: Parser[Text] =
    """\(Text: """.r ~ repsep(textEl, ",") ~ """\)""".r ^^ {
      case s1 ~ teList ~ s2 => Text(teList)
    }

  private def textEl: Parser[TextEl] = headline | paragraph

  def headline: Parser[Headline] =
    """\(Headline """.r ~ hNum ~ """: """.r ~ wrappedIdentifier ~ """\)""".r ^^ {
      case s1 ~ hn ~ s2 ~ id ~ s3 => Headline(id, hn)
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
    """\(List ordered:""".r ~ repsep(listElement, ",") ~ """\)""".r ^^ { case s1 ~ listElList ~ s2 => OrderedList(listElList)
    }

  def listElement: Parser[ListElement] = wrappedIdentifier ^^ { id => ListElement(id) }

  def fullTable: Parser[Table] =
    """\(Table:""".r ~ tableRowHead ~ """,""".r ~ repsep(tableRowData, ",") ~ """\)""".r ^^ {
      case s1 ~ trh ~ s2 ~ trdList ~ s3 => Table(trh, trdList)
    }

  def tableRowHead: Parser[Tablerowhead] =
    """\(Tablerow:""".r ~ repsep(tablehead, ",") ~ """\)""".r ^^ {
      case s1 ~ thList ~ s2 => Tablerowhead(thList)
    }

  def tableData: Parser[Tabledata] = wrappedIdentifier ^^ { id => Tabledata(id) }

  def tableRowData: Parser[Tablerowdata] =
    """\(Tablerow:""".r ~ repsep(tableData, ",") ~ """\)""".r ^^ {
      case s1 ~ tdList ~ s2 => Tablerowdata(tdList)
    }

  def tablehead: Parser[Tablehead] =
    """\(""".r ~ word ~ """\)""".r ^^ { case s1 ~ id ~ s2 => Tablehead(id)
    }

  def form: Parser[BodyElement] =
    """\(Form:""".r ~ repsep(formElWLabel, ",") ~ """\)""".r ^^ { case s1 ~ feList ~ s2 => Form(feList); }

  def formElWLabel: Parser[FormEl] = label ~ """,""".r ~ formEl ^^ {
    case la ~ s ~ el => FormEl(la, el)
  }

  def formEl: Parser[FormElEl] = input | textArea

  def label: Parser[Label] = """\(Label:""".r ~ formId ~ """,""".r ~ wrappedIdentifier ~ """\)""".r ^^ { case s1 ~ id ~ s2 ~ wi ~ s3 => Label(id, wi) }

  def input: Parser[InputEl] =
    """\(Input:""".r ~ formId ~ """,""" ~ placeHolder ~ """\)""".r ^^ { case s1 ~ fi ~ s2 ~ ph ~ s3 => InputEl(fi, ph) }

  def placeHolder: Parser[Placeholder] = """\(Placeholder:""".r ~ wrappedIdentifier ~ """\)""".r ^^ { case s1 ~ id ~ s2 => Placeholder(id) }

  def formId: Parser[FormIdentifier] =
    """\(Id:""".r ~ wrappedIdentifier ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 => FormIdentifier(id)
    }

  def textArea: Parser[TextArea] =
    """\(Textarea:""".r ~ formId ~ """,""" ~ placeHolder ~ """\)""".r ^^ { case s1 ~ fi ~ s2 ~ ph ~ s3 => TextArea(fi, ph) }

  private def wrappedIdentifier: Parser[String] =
    """\(""".r ~ identifier ~ """\)""".r ^^ {
      case s1 ~ id ~ s2 => id
    }

  private def hNum: Parser[Int] =
    """[1-4]""".r ^^ {
      _.toInt
    }

  private def imageIdentifier: Parser[String] =
    """(([/\-!:,;'&_a-zA-Z01-9öäü\d\s])+)""".r

  private def identifier: Parser[String] =
    """(([/\-!.:,;'&_a-zA-Z01-9öäü\d\s])+)""".r

  def word: Parser[String] =
    """([_a-zA-Z]+)|(0|[1-9]\d*)""".r

  override protected val whiteSpace: Regex = """\s*|//.*""".r

}

object WebsiteParser {

  case class Image(identifier: String) extends BodyElement {
    override def toHtml: String = "<img src=\"" + identifier + "\">\n"

    override def toString: String = "(Image:(" + identifier + "))"
  }

  case class Website(pages: List[Page]) {
    def analyzeSemantics(test: Boolean): Any = {
      var failure: String = ""
      for (page <- pages) {
        for (el <- page.body.bodyElements) {
          el match {
            case form: Form =>
              for (ele <- form.formEls) {
                if (ele.label.toString != ele.formEl.id.toString) {
                  failure = "Error: Label and Form Identifier must match!"
                  if (!test) {
                    println(failure)
                    exit(99)
                  } else {
                    return failure
                  }
                }
              }
            case table: Table =>
              var rs: Int = 0
              var hs: Int = 0
              for (heads <- table.tablerowhead.tableheads) {
                hs = hs + 1
              }
              for (rows <- table.tablerowdatas) {
                for (data <- rows.tabledatas) {
                  rs = rs + 1
                }
                if (hs != rs) failure = "Error: All table rows must have the same number as table columns!"
                if (!test) {
                  println(failure)
                  exit(99)
                } else {
                  return failure
                }
              }
            case _ =>
          }
        }
        if (page.header.navbar.elements.length > 10) {
          failure = "Error: To provide an optimal overview, the navbar may only contain 10 elements or less."
          if (!test) {
            println(failure)
            exit(99)
          } else {
            return failure
          }
        }
      }
    }

    def buildWebsite(): Unit = {
      var writer: Writer = new Writer()
      var nr = 1
      analyzeSemantics(false)
      for (page <- pages) {
        writer.writeFile(page.toHtml, nr)
        nr += 1
      }
    }

    override def toString: String = "Website: " + pages.mkString(", ")
  }

  case class Page(header: Header, body: Body, footer: Footer) {
    def toHtml: String = "<!DOCTYPE html>\n<html lang=\"de\">\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">\n<meta charset=\"utf-8\">\n<link rel=\"stylesheet\" href=\"misc/bootstrap.css\">\n<link rel=\"stylesheet\" href=\"misc/my.css\">\n<script src=\"misc/jquery.js\"></script>\n<script src=\"misc/bootstrap.js\"></script>\n</head>\n" +
      header.toHtml + body.toHtml + footer.toHtml + "</html>"

    override def toString: String = "(Page: " + header.toString() + ", " + body.toString() + ", " + footer.toString() + ")"
  }

  case class Header(image: Image, navbar: Navbar) {
    def toHtml: String = "<header>\n<div class=\"jumbotron\">\n<div class=\"container text-left\">\n" + image.toHtml + "</div>\n</div>\n" + navbar.toHtml + "</header>\n"

    override def toString: String = "(Header: " + image.toString() + ", " + navbar.toString() + ")"
  }

  case class Body(bodyElements: List[BodyElement]) {
    private val htmlBuilder = new StringBuilder()
    for (el <- bodyElements) {
      htmlBuilder.append(el.toHtml)
    }

    def toHtml: String = "<body>\n" + htmlBuilder.toString() + "</body>\n"

    override def toString: String = "(Body: " + bodyElements.mkString(", ") + ")"
  }

  sealed abstract class BodyElement() {
    def toHtml: String
  }

  case class Footer(links: List[Link]) {
    private val htmlBuilder = new StringBuilder()
    for (link <- links) {
      htmlBuilder.append("<li>\n").append(link.toHtml).append("</li>")
    }

    def toHtml: String = "<footer class=\"container-fluid text-center\">\n<ul>\n" + htmlBuilder.toString() +
      "</ul>\n</footer>\n"

    override def toString: String = "(Footer: " + links.mkString(", ") + ")"
  }

  case class Navbar(elements: List[NavbarElement]) {
    private val htmlBuilder = new StringBuilder()
    for (el <- elements) {
      htmlBuilder.append(el.toHtml)
    }

    def toHtml: String = "<nav class=\"navbar\">\n<div class=\"container\">\n<div class=\"collapse navbar-collapse\" id=\"myNavbar\">\n" +
      "<ul class=\"nav navbar-nav\">\n" + htmlBuilder.toString() + "</ul>\n</div>\n</div>\n</nav>\n"

    override def toString: String = "(Navbar: " + elements.mkString(", ") + ")"
  }

  sealed abstract class NavbarElement() {
    def toHtml: String
  }

  case class Link(destination: Destination, identifier: LinkIdentifier) extends BodyElement {
    override def toHtml: String = "<a href=\"" + destination + "\">" + identifier + "</a>\n"

    override def toString: String = "(Link: (" + identifier + "), (" + destination + "))"
  }

  case class NavLink(destination: Destination, identifier: LinkIdentifier) extends NavbarElement {
    override def toHtml: String = "<li><a href=\"" + destination + "\">" + identifier + "</a></li>\n"

    override def toString: String = "(Link: (" + identifier + "), (" + destination + "))"
  }

  case class Destination(identifier: String) {
    override def toString: String = identifier
  }

  case class LinkIdentifier(identifier: String) {
    override def toString: String = identifier
  }

  case class NavbarList(id: String, links: List[NavLink]) extends NavbarElement {
    private val htmlBuilder = new StringBuilder()
    for (link <- links) {
      htmlBuilder.append(link.toHtml)
    }

    override def toHtml: String = "<li class=\"dropdown\">\n<a class=\"dropdown-toggle\" data-toggle=\"dropdown\">" + id +
      "\n<span class=\"caret\"></span></a>\n<ul class=\"dropdown-menu\">\n" + htmlBuilder.toString() + "</ul>\n</li>\n"

    override def toString: String = "(Dropdown: (" + id + "), " + links.mkString(", ") + ")"
  }

  case class Text(textel: List[TextEl]) extends BodyElement {
    private val htmlBuilder = new StringBuilder()
    for (el <- textel) {
      htmlBuilder.append(el.toHtml)
    }

    override def toHtml: String = "<div class=\"container-fluid text-center\">\n<div class=\"col-sm-2 sidenav\">\n</div>\n<div class=\"col-sm-8 text-left bg-content\">\n" + htmlBuilder.toString() + "</div>\n</div>\n"

    override def toString: String = "(Text: " + textel.mkString(", ") + ")"
  }

  sealed abstract class TextEl() {
    def toHtml: String
  }

  case class Headline(identifier: String, num: Int) extends TextEl {
    override def toHtml: String = "<h" + num.toString + ">" + identifier + "</h" + num.toString + ">\n"

    override def toString: String = "(Headline " + num + ": (" + identifier + "))"
  }

  case class Paragraph(identifier: String) extends TextEl {
    override def toHtml: String = "<p style=\"margin-bottom: 25px\">" + identifier + "</p>\n"

    override def toString: String = "(Paragraph: (" + identifier + "))"
  }

  case class UnorderedList(elements: List[ListElement]) extends BodyElement {
    private val htmlBuilder = new StringBuilder()
    for (el <- elements) {
      htmlBuilder.append(el.toHtml)
    }

    override def toHtml: String = "<div class=\"col-sm-8 text-left bg-content\">\n<ul>" + htmlBuilder.toString() + "</ul></div>\n"

    override def toString: String = "(List unordered: " + elements.mkString(", ") + ")"
  }

  case class OrderedList(elements: List[ListElement]) extends BodyElement {
    private val htmlBuilder = new StringBuilder()
    for (el <- elements) {
      htmlBuilder.append(el.toHtml)
    }

    override def toHtml: String = "<div class=\"col-sm-8 text-left bg-content\">\n<ol>" + htmlBuilder.toString() + "</ol></div>\n"

    override def toString: String = "(List ordered: " + elements.mkString(", ") + ")"
  }

  case class ListElement(identifier: String) {
    def toHtml: String = "<li>" + identifier + "</li>\n"

    override def toString: String = "(" + identifier + ")"
  }

  case class Table(tablerowhead: Tablerowhead, tablerowdatas: List[Tablerowdata]) extends BodyElement {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (trd <- tablerowdatas) {
      htmlBuilder.append(trd.toHtml).append("\n")
    }

    override def toHtml: String = "<div class=\"col-sm-10 text-center bg-content\">\n<table class=\"table\">\n" + tablerowhead.toHtml + "\n" + htmlBuilder.toString() + "</table>\n</div>\n"

    override def toString: String = "(Table: " + tablerowhead + ", " + tablerowdatas.mkString(", ") + ")"

  }

  case class Tablerowdata(tabledatas: List[Tabledata]) {
    private val htmlBuilder = new StringBuilder()
    for (td <- tabledatas) {
      htmlBuilder.append(td.toHtml)
    }

    def toHtml: String = "<tr>\n" + htmlBuilder.toString() + "</tr>"

    override def toString: String = "(Tablerow: " + tabledatas.mkString(", ") + ")"
  }

  case class Tablerowhead(tableheads: List[Tablehead]) {
    private val htmlBuilder = new StringBuilder()
    for (th <- tableheads) {
      htmlBuilder.append(th.toHtml)
    }

    def toHtml: String = "<thead>\n<tr>\n" + htmlBuilder.toString() + "</tr>\n</thead>"

    override def toString: String = "(Tablerow: " + tableheads.mkString(", ") + ")"
  }

  case class Tablehead(identifier: String) {
    def toHtml: String = "<th class=\"text-center\">" + identifier + "</th>\n"

    override def toString: String = "(" + identifier + ")"
  }

  case class Tabledata(identifier: String) {
    def toHtml: String = "<td>" + identifier + "</td>\n"

    override def toString: String = "(" + identifier + ")"
  }

  private case class Form(formEls: List[FormEl]) extends BodyElement {
    private val sb = new StringBuilder()
    private val htmlBuilder = new StringBuilder()
    for (el <- formEls) {
      sb.addString(sb.append("(" + el + ")"), ",")
      htmlBuilder.append(el.toHtml)
    }

    override def toHtml: String = "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">" +
      "\n" + htmlBuilder.toString() + "<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>"

    override def toString: String = "(Form: " + sb.toString() + ")"
  }

  case class FormEl(label: Label, formEl: FormElEl) {
    def toHtml: String = label.toHtml + "\n" + formEl.toHtml + "\n"

    override def toString: String = "(" + label.toString() + "," + formEl.toString + ")"
  }

  abstract sealed class FormElEl() {
    def id: FormIdentifier

    def toHtml: String
  }

  case class Label(id: FormIdentifier, identifier: String) {
    def toHtml: String = "<label for=\"" + id + "\">" + identifier + "</label>\n"

    override def toString: String = "(Label: (Id: (" + id.toString() + ")), (" + identifier + "))"
  }

  case class TextArea(id: FormIdentifier, placeholder: Placeholder) extends FormElEl {
    override def toHtml: String = "<textarea style=\"margin-bottom: 50px\" id=\"" + id.toString + "\" class=\"form-control\" placeholder=\"" + placeholder.toString + "\" style=\"height:200px\"></textarea>\n"

    override def toString: String = "(Textarea: (Id: (" + id.toString + ")), (Placeholder: (" + placeholder.toString + ")))"

  }

  case class Placeholder(identifier: String) {
    override def toString: String = identifier
  }

  case class FormIdentifier(id: String) {
    override def toString: String = id
  }

  case class InputEl(id: FormIdentifier, placeholder: Placeholder) extends FormElEl {
    override def toHtml: String = "<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"" + id.toString + "\" placeholder=\"" + placeholder.toString + "\">"

    override def toString: String = "(Input: (Id: (" + id + ")), (Placeholder: (" + placeholder.toString + ")))"

  }
}