package xieyuheng.cicada

import xieyuheng.partech
import xieyuheng.partech._

object parserTest extends App {
  import parser._

  val sentences = Seq(
    "type",
    "nat_t",
    "x.prev.prev",
    "x .prev .prev",
    "x . prev . prev",
  )

  val non_sentences = Seq(
    "x . .",
  )

  val rule = exp

  sentences.foreach { case text =>
    Parser(rule).parsing(text).nextTree match {
      case Some(tree) =>
        println(partech.pretty.prettyTree(tree))
      case None =>
        println(s"[parserTest] should parse")
        println(s"- rule: ${rule.name}")
        println(s"- text: ${text}")
        throw new Exception()
    }
  }

  non_sentences.foreach { case text =>
    Parser(rule).parsing(text).nextTree match {
      case Some(tree) =>
        println(s"[parserTest] should not parse")
        println(s"- rule: ${rule.name}")
        println(s"- text: ${text}")
        println(s"- tree: ${partech.pretty.prettyTree(tree)}")
        throw new Exception()
      case None => {}
    }
  }
}
