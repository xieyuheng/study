package xieyuheng.partech

import scala.collection.mutable.ListBuffer

case class Parser(rule: Rule, lexer: Lexer) {
  def parse(text: String): Either[ErrMsg, Tree] = {
    lexer.lex(text).flatMap { case words =>
      val parsing = Earley.init(words, rule)
      parsing.parse()
    }
  }
}
