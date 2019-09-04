package xieyuheng.partech

import scala.util.control.Breaks._

case class LexTable(
  wordMatcher: String => Option[(String, String)],
  ignorer: String => String)

case class Span(lo: Int, hi: Int)
case class Word(str: String, span: Span)

case class Lexer(table: LexTable) {
  def lex(text: String): Either[ErrMsg, Seq[Word]] = {
    var remain: String = text
    var tokens: Seq[Word] = Seq()
    var maybeErr: Option[ErrMsg] = None

    breakable {
      while (remain.length > 0) {
        remain = table.ignorer(remain)

        table.wordMatcher(remain) match {
          case Some((left, right)) =>
            val hi = text.length - right.length
            val lo = hi - left.length
            tokens :+ Word(left, Span(lo, hi))
            remain = right
          case None =>
            val hi = text.length
            val lo = text.length - remain.length
            Left(ErrMsg(
              tag = "Lexer",
              msg = s"",
              text = text,
              span = Span(lo, hi)))
        }
      }
    }

    maybeErr match {
      case Some(err) => Left(err)
      case None => Right(tokens)
    }
  }
}
