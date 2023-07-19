package Generation

import scala.util.parsing.combinator._

import scala.language.postfixOps
import scala.util.matching.Regex

class WebsiteParser extends RegexParsers {

  import Absyn._

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

  private def bodyEl: Parser[BodyElement] = image | text | unorderedList |
                                            orderedList | fullTable | link | form

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

  def destination: Parser[Destination] = subpage | mail

  def subpage: Parser[Destination] = """\(""".r ~ word ~ """\.html""".r ~ """\)""".r ^^  {
  case s1 ~ id ~ s2 ~ s3 => Destination(id + s2) }

  def mail: Parser[Destination] = """\(""".r ~ """mailto:""".r ~ identifier ~ """\)""".r ^^ {
    case s1 ~ s2 ~ id ~ s3 => Destination(s2 + id)
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

  def tablehead: Parser[Tablehead] =
    """\(""".r ~ word ~ """\)""".r ^^ { case s1 ~ id ~ s2 => Tablehead(id)
    }

  def tableRowData: Parser[Tablerowdata] =
    """\(Tablerow:""".r ~ repsep(tableData, ",") ~ """\)""".r ^^ {
      case s1 ~ tdList ~ s2 => Tablerowdata(tdList)
    }

  def tableData: Parser[Tabledata] = wrappedIdentifier ^^ { id => Tabledata(id) }


  def form: Parser[BodyElement] =
    """\(Form:""".r ~ repsep(formElWLabel, ",") ~ """\)""".r ^^ { case s1 ~ feList ~ s2 => Form(feList); }

  private def formElWLabel: Parser[FormElEl] = label ~ """,""".r ~ formEl ^^ {
    case la ~ s ~ el => FormElEl(la, el)
  }

  def formEl: Parser[FormEl] = input | textArea

  def label: Parser[Label] = """\(Label:""".r ~ formId ~ """,""".r ~ wrappedIdentifier ~ """\)""".r ^^ { case s1 ~ id ~ s2 ~ wi ~ s3 => Label(id, wi) }

  def input: Parser[InputEl] =
    """\(Input:""".r ~ formId ~ """,""" ~ placeHolder ~ """\)""".r ^^ { case s1 ~ fi ~ s2 ~ ph ~ s3 => InputEl(fi, ph) }

  private def placeHolder: Parser[Placeholder] = """\(Placeholder:""".r ~ wrappedIdentifier ~ """\)""".r ^^ { case s1 ~ id ~ s2 => Placeholder(id) }

  private def formId: Parser[FormIdentifier] =
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
    """(([/\-!.:,;'@ß&_a-zA-Z01-9öäü\d\s])+)""".r

  private def word: Parser[String] =
    """([_öäüßa-zA-Z01-9]+)""".r

  override protected val whiteSpace: Regex = """\s*|//.*""".r

}