package xieyuheng.partech

import scala.collection.mutable.ListBuffer

case class Parser(rule: Rule, lexer: Lexer) {

  def parse(text: String): Either[ErrMsg, Tree] = {
    lexer.lex(text).flatMap { case words =>
      val parsing = Parsing(text, words, ListBuffer((0, List(), List(LinearTreePartRule(rule)))))
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

case class Parsing(
  text: String,
  words: List[Word],
  queue: ListBuffer[(Int, List[LinearTreePart], List[LinearTreePart])],
) {

  def nextLinearTree(): Option[List[LinearTreePart]] = {
    var result: Option[List[LinearTreePart]] = None
    var continue: Boolean = true

    while (continue) {
      queue.headOption match {
        case None => continue = false
        case Some((i, left, right)) => {
          queue.trimStart(1)

          if (right.isEmpty) {
            if (i == words.length) {
              result = Some(left)
              continue = false
            }
          } else {
            val head = right.head
            val tail = right.tail

            head match {
              case LinearTreePartStr(str) =>
                val word = words(i)
                if (word.str == str) {
                  queue.prepend((i + 1, left :+ head, tail))
                }
              case LinearTreePartRule(rule) =>
                val linearTreeList = rule.choices
                  .map { case (choiceName, ruleParts) =>
                    List(LinearTreePartBra(rule, choiceName)) ++
                    ruleParts.map(LinearTreePart.fromRulePart) ++
                    List(LinearTreePartKet(rule, choiceName)) }
                  .filter { case parts =>
                    // simply pruning by lower bound of length
                    // - pruning is always heuristic,
                    // - pruning should balance with searching
                    parts.map(_.lowerBound).sum + tail.map(_.lowerBound).sum <= words.length - i }
                  .map { case parts => (i, left, parts ++ tail) }
                queue.appendAll(linearTreeList)
              case LinearTreePartBra(rule, choiceName) =>
                queue.prepend((i, left :+ head, tail))
              case LinearTreePartKet(rule, choiceName) =>
                queue.prepend((i, left :+ head, tail))
              case LinearTreePartPred(pred) =>
                val word = words(i)
                if (pred(word.str)) {
                  queue.prepend((i + 1, left :+ LinearTreePartStr(word.str), tail))
                }
            }
          }
        }
      }
    }

    result
  }

  def nextTree(): Option[Tree] = {
    nextLinearTree().flatMap { case parts =>
      Some(Tree.fromLinearTree(parts))
    }
  }
}
