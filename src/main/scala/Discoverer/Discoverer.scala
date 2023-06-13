package Discoverer

import Parser.WebsiteParser._

class Discoverer() {
  var input: String = ""
  var output: String = ""

  def discover(): Unit = {
    if (input.contains("input")) {
      var sub: String = input.replace(input, input.substring(72))
      var id: String = ""
      var placeholder: String = ""
      println(sub)
      while(sub.charAt(0) != '"') {
        id = id + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      println(id)
      println(sub)
      sub = sub.replace(sub, sub.substring(15))
      println(sub)
      while (sub.charAt(0) != '"') {
        placeholder = placeholder + sub.charAt(0)
        sub = sub.replace(sub, sub.substring(1))
      }
      println(placeholder)
      val formIdentifier: FormIdentifier = FormIdentifier(id)
      val ph: Placeholder = Placeholder(placeholder)
      val inputEl: InputEl = InputEl(formIdentifier, ph)
      println(inputEl)
    }
  }
}
