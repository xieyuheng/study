package xieyuheng.partech

import xieyuheng.partech.example._

object ParserTest extends App {
  val rule = exp.start
  val lexer = exp.lexer

  val sentences = List(
    "type",
    "n",
    "succ_t(nat_add(x = x.prev, y = y))",
    """
    x case {
      zero_t => y
      succ_t => succ_t(nat_add(x = x.prev, y = y))
    }
    """,
  )

  sentences.foreach { case text =>
    Parser(rule, lexer).parse(text) match {
      case Right(tree) =>
        println(tree)
      case Left(error) =>
        println(s"[ParserTest] should parse")
        println(s"- rule: ${rule.name}")
        println(s"- text: ${text}")
        println(s"- error: ${error}")
        throw new Exception()
    }
  }

}
