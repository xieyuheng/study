package xieyuheng.partech

import scala.collection.mutable.ListBuffer

case class Parser(rule: Rule, lexer: Lexer) {
  def parse(text: String): Either[ErrMsg, Tree] = {
    lexer.lex(text).flatMap { case words =>
      val parsing = LL1_plus_depth_first_search.init(words, rule)
      parsing.nextTree() match {
        case Some(parts) => Right(parts)
        case None =>
          val lo = 0
          val hi = text.length
          Left(ErrMsg(
            tag = "Parser",
            msg = s"",
            text = text,
            span = Span(lo, hi)))
      }
    }
  }
}
