package xieyuheng.cicada.with_logic_variable

import xieyuheng.partech._

object parser {

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

  def lexer = Lexer(LexTable(wordMatcher, ignorer))

}
