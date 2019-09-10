package xieyuheng.partech //

case class LexTable(
  wordMatcher: String => Option[(String, String)],
  ignorer: String => String)

case class Span(lo: Int, hi: Int)
case class Word(str: String, span: Span) {
  override def toString: String = {
    '"' + str + '"' ++ s" [${span.lo}, ${span.hi}]"
  }
}

case class Lexer(table: LexTable) {
  def lex(text: String): Either[ErrMsg, List[Word]] = {
    var remain: String = text
    var tokens: List[Word] = List()
    var maybeErr: Option[ErrMsg] = None

    while (remain.length > 0) {
      remain = table.ignorer(remain)

      table.wordMatcher(remain) match {
        case Some((left, right)) =>
          val hi = text.length - right.length
          val lo = hi - left.length
          tokens = tokens :+ Word(left, Span(lo, hi))
          remain = right
        case None =>
          val hi = text.length
          val lo = text.length - remain.length
          Left(ErrMsg("Lexer", s"", Span(lo, hi)))
      }
    }

    maybeErr match {
      case Some(err) => Left(err)
      case None => Right(tokens)
    }
  }
}

object Lexer {

  val space_chars = Set(' ', '\n', '\t')

  def ignoreSpace(text: String): Option[String] = {
    text.headOption match {
      case Some(char) =>
        if (space_chars.contains(char)) {
          Some(text.tail)
        } else {
          None
        }
      case _ => None
    }
  }

  def ignoreLineComment(text: String): Option[String] = {
    if (text.startsWith("//")) {
      text.indexOf('\n') match {
        case -1 => Some("")
        case n =>
          val (_, remain) = text.splitAt(n)
          Some(remain)
      }
    } else {
      None
    }
  }

  def ignorer(text: String): String = {
    var remain: String = text
    var continue: Boolean = true
    while (continue) {
      ignoreSpace(remain) match {
        case Some(str) => remain = str
        case None => ignoreLineComment(remain) match {
          case Some(str) => remain = str
          case None => continue = false
        }
      }
    }
    remain
  }

  val symbol_chars = Set(
    '=', ':', '.',
    ',', ';',

    '~', '!', '@', '#', '$', '%', '^', '&', '*',  '-', '+',

    '<', '>',
    '(', ')',
    '[', ']',
    '{', '}',
  )

  def wordMatcher(text: String): Option[(String, String)] = {
    text.headOption match {
      case Some(char) =>
        if (symbol_chars.contains(char)) {
          Some((char.toString, text.tail))
        } else {
          text.find { case char =>
            space_chars.contains(char) ||
            symbol_chars.contains(char)
          } match {
            case Some(char) =>
              val i = text.indexOf(char)
              Some(text.splitAt(i))
            case None =>
              Some((text, ""))
          }
        }
      case None => None
    }
  }

  def default = Lexer(LexTable(wordMatcher, ignorer))

}
