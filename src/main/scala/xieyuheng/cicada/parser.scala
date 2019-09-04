package xieyuheng.cicada

import xieyuheng.partech._

import scala.util.control.Breaks._

object parser {

  def ignoreSpace(text: String): Option[String] = {
    text.headOption match {
      case Some(' ') => Some(text.tail)
      case Some('\n') => Some(text.tail)
      case Some('\t') => Some(text.tail)
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

  def wordMatcher = ???

  def lexer = Lexer(LexTable(wordMatcher, ignorer))

}
