package Discoverer

import Parser.WebsiteParser._

import java.util

class Discoverer() {
  var input: String = ""

  def discover(): Object = {
    // Input
    if (input.contains("input")) {
      return discoverInput()
    } // TextArea
    else if (input.contains("textarea")) {
      return discoverTextarea()
    } // Label
    else if (input.contains("label")) {
      return discoverLabel()
    } // Tablehead
    else if (input.contains("th")) {
      return discoverTablehead()
    } // TableRowData
    else if (input.contains("tr")) {
      return discoverTableRowData()
    } // TableData*/
    else if (input.contains("td")) {
      return discoverTabledata(input)
    }
    ""
  }

  def discoverInput(): InputEl = {
    var sub: String = input.replace(input, input.substring(72))
    var id: String = ""
    var placeholder: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    sub = sub.replace(sub, sub.substring(15))
    while (sub.charAt(0) != '"') {
      placeholder = placeholder + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val formIdentifier: FormIdentifier = FormIdentifier(id)
    val ph: Placeholder = Placeholder(placeholder)
    val inputEl: InputEl = InputEl(formIdentifier, ph)
    println(inputEl.toString)
    assert(inputEl.toString == "(Input: (Id: (fname)), (Placeholder: (Vorname)))")
    inputEl
  }

  def discoverTextarea(): TextArea = {
    var sub: String = input.replace(input, input.substring(42))
    var id: String = ""
    var placeholder: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    sub = sub.replace(sub, sub.substring(36))
    while (sub.charAt(0) != '"') {
      placeholder = placeholder + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val formIdentifier: FormIdentifier = FormIdentifier(id)
    val ph: Placeholder = Placeholder(placeholder)
    val textArea: TextArea = TextArea(formIdentifier, ph)
    println(textArea)
    textArea
  }

  def discoverLabel(): Label = {
    var sub: String = input.replace(input, input.substring(12))
    var id: String = ""
    var in: String = ""
    while (sub.charAt(0) != '"') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    sub = sub.replace(sub, sub.substring(2))
    while (sub.charAt(0) != '<') {
      in = in + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val formIdentifier: FormIdentifier = FormIdentifier(id)
    val label: Label = Label(formIdentifier, in)
    println(label)
    label
  }

  def discoverTablehead(): Tablehead = {
    var sub: String = input.replace(input, input.substring(24))
    var id: String = ""
    while (sub.charAt(0) != '<') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val tablehead: Tablehead = Tablehead(id)
    println(tablehead)
    tablehead
  }

  def discoverTableRowData(): String = {
    var sub: String = input.replace(input, input.substring(5))
    println(sub)
    var end: String = "</tr>"
    var tabledata: String = ""
    var tabledatas: List[Tabledata] = List()
    while(sub != end) {
      while (sub.charAt(0) != '\n') {
        tabledata = tabledata + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      var td: Tabledata = discoverTabledata(tabledata)
      tabledatas = tabledatas ++ List(td)
    }
    sub
  }

  def discoverTabledata(input: String): Tabledata = {
    var sub: String = input.replace(input, input.substring(4))
    var id: String = ""
    while (sub.charAt(0) != '<') {
      id = id + sub.charAt(0)
      sub = sub.replace(sub, sub.substring(1))
    }
    val tabledata: Tabledata = Tabledata(id)
    println(tabledata)
    tabledata
  }
}
