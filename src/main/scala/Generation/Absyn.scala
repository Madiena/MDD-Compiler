package Generation

import Utils.Writer

import scala.sys.exit

/**
 * Object that defines the abstract syntax tree and its elements.
 */
object Absyn {

  case class Image(identifier: String) extends BodyElement {
    override def toHtml: String = "<img src=\"" + identifier + "\">\n"

    override def toString: String = "(Image:(" + identifier + "))"
  }

  case class Website(pages: List[Page]) {

    /**
     * Helper method to handle semantic errors.
     * @param error Error message that has to be printed.
     * @param test Boolean that defines whether method is used within a test or not
     * @return Error message.
     */
    private def failure(error: String, test: Boolean): String = {
      val failure: String = error
      if (!test) {
        println(failure)
        exit(99)
      } else {
        failure
      }
    }

    /**
     * Method to analyze a models for restrictions and handle errors caused by violating model restrictions.
     *
     * @param test  Boolean that defines whether method is used within a test or not
     * @return Error message.
     */
    def analyzeSemantics(test: Boolean): Any = {
      if (pages.isEmpty) {
        return failure("Error: At least one page must be provided!\n", test)
      }
      for (page <- pages) {
        for (el <- page.body.bodyElements) {
          el match {
            case form: Form =>
              for (ele <- form.formEls) {
                if (ele.label.id.toString != ele.formEl.id.toString) {
                  return failure("Error: Label and form identifier must match!\n", test)
                }
              }
              if (form.formEls.isEmpty) {
                return failure("Error: At least one form element must be provided within a form!\n", test)
              }
            case table: Table =>
              var rs: Int = 0
              var hs: Int = 0
              for (_ <- table.tablerowhead.tableheads) {
                hs = hs + 1
              }
              for (rows <- table.tablerowdatas) {
                for (_ <- rows.tabledatas) {
                  rs = rs + 1
                }
                if (hs != rs) {
                  return failure("Error: All table rows must have the same number as table columns!\n", test)
                }
                rs = 0
              }
              if (table.tablerowdatas.isEmpty) {
                return failure("Error: At least one row of table heads and one row of table datas must be provided within a table.\n", test)
              }
            case text: Text => if (text.textel.isEmpty) {
              return failure("Error: At least one text element must be provided within a text!\n", test)
            }
            case ul: UnorderedList => if (ul.elements.isEmpty) {
              return failure("Error: At least one list element must be provided within the unordered list!\n", test)
            }
            case ol: OrderedList => if (ol.elements.isEmpty) {
              return failure("Error: At least one list element must be provided within the ordered list!\n", test)
            }
            case _ =>
          }
        }
        if (page.header.navbar.elements.length > 10) {
          return failure("Error: To provide an optimal overview, the navbar may only contain 10 elements or less.\n", test)
        } else if (page.header.navbar.elements.isEmpty) {
          return failure("Error: At least one element must be provided within the navbar!\n", test)
        }
        for (el <- page.header.navbar.elements) {
          el match {
            case navbarList: NavbarList => if (navbarList.links.isEmpty) {
              return failure("Error: At least one link must be provided within the navbar list!\n", test)
            }
            case _ =>
          }
        }
        if (page.body.bodyElements.isEmpty) {
          return failure("Error: At least one element must be provided within the body!\n", test)
        }
        if (page.footer.links.isEmpty) {
          return failure("Error: At least one link must be provided within the footer!\n", test)
        }
      }
    }

    /**
     * Method to build website.
     */
    def buildWebsite(): Unit = {
      val writer: Writer = new Writer()
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

    def toHtml: String = "<body>\n<div class=\"container-fluid text-center\">\n<div class=\"row content\">\n\n<div class=\"col-sm-2 sidenav\">\n\n</div>\n\n<div class=\"col-sm-8 text-left bg-content container\">" + htmlBuilder.toString() + "</div></div></div></body>\n"

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
    override def toHtml: String = "<div><a href=\"" + destination + "\">" + identifier + "</a>\n</div>\n"

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

    override def toHtml: String = htmlBuilder.toString()

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
    private val htmlBuilder = new StringBuilder()
    for (trd <- tablerowdatas) {
      htmlBuilder.append(trd.toHtml).append("\n")
    }

    override def toHtml: String = "<div class=\"col-sm-10 text-center bg-content\">\n<table class=\"table\">\n" +
      tablerowhead.toHtml + "\n" + htmlBuilder.toString() + "</table>\n</div>\n"

    override def toString: String = "(Table: " + tablerowhead + ", " + tablerowdatas.mkString(", ") + ")"

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

  case class Tablerowdata(tabledatas: List[Tabledata]) {
    private val htmlBuilder = new StringBuilder()
    for (td <- tabledatas) {
      htmlBuilder.append(td.toHtml)
    }

    def toHtml: String = "<tr>\n" + htmlBuilder.toString() + "</tr>"

    override def toString: String = "(Tablerow: " + tabledatas.mkString(", ") + ")"
  }
  case class Tabledata(identifier: String) {
    def toHtml: String = "<td>" + identifier + "</td>\n"

    override def toString: String = "(" + identifier + ")"
  }

   case class Form(formEls: List[FormElEl]) extends BodyElement {
    private val htmlBuilder = new StringBuilder()
    for (el <- formEls) {
      htmlBuilder.append(el.toHtml)
    }

    override def toHtml: String = "<div class=\"col-sm-8 text-left bg-content container\">\n<form action=\"action_page.php\" style=\"width:600px\">\n<div class=\"form-group\" style=\"margin-top: 50px\">" +
      "\n" + htmlBuilder.toString() + "<input type=\"submit\" value=\"Submit\">\n</div>\n</form>\n</div>"

    override def toString: String = "(Form: " + formEls.mkString(", ") + ")"
  }

  case class FormElEl(label: Label, formEl: FormEl) {
    def toHtml: String = label.toHtml + "\n" + formEl.toHtml + "\n"

    override def toString: String = label.toString() + ", " + formEl.toString
  }

  abstract sealed class FormEl() {
    def id: FormIdentifier

    def toHtml: String
  }

  case class Label(id: FormIdentifier, identifier: String) {
    def toHtml: String = "<label for=\"" + id + "\">" + identifier + "</label>\n"

    override def toString: String = "(Label: (Id: (" + id.toString() + ")), (" + identifier + "))"
  }

  case class TextArea(id: FormIdentifier, placeholder: Placeholder) extends FormEl {
    override def toHtml: String = "<textarea style=\"margin-bottom: 50px\" id=\"" + id.toString + "\" class=\"form-control\" placeholder=\"" + placeholder.toString + "\" style=\"height:200px\"></textarea>\n"

    override def toString: String = "(Textarea: (Id: (" + id.toString + ")), (Placeholder: (" + placeholder.toString + ")))"

  }

  case class Placeholder(identifier: String) {
    override def toString: String = identifier
  }

  case class FormIdentifier(id: String) {
    override def toString: String = id
  }

  case class InputEl(id: FormIdentifier, placeholder: Placeholder) extends FormEl {
    override def toHtml: String = "<input style=\"margin-bottom: 25px\" type=\"text\" class=\"form-control\" id=\"" + id.toString + "\" placeholder=\"" + placeholder.toString + "\">"

    override def toString: String = "(Input: (Id: (" + id + ")), (Placeholder: (" + placeholder.toString + ")))"

  }
}
