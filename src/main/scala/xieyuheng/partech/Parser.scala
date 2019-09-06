package xieyuheng.partech

import scala.collection.mutable.ListBuffer

case class Parser(rule: Rule, lexer: Lexer) {

  def parse(text: String): Either[ErrMsg, Tree] = {
    lexer.lex(text).flatMap { case words =>
      val parsing = Parsing(text, words, ListBuffer(ParsingFrame.init(words, rule)))
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

case class ParsingFrame(
  leftIndex: Int,
  left: List[LinearTreePart],
  remain: List[LinearTreePart],
  rightIndex: Int,
  right: List[LinearTreePart])

object ParsingFrame {
  def init(words: List[Word], rule: Rule): ParsingFrame = {
    ParsingFrame(
      leftIndex = 0,
      left = List(),
      remain = List(LinearTreePartRule(rule)),
      rightIndex = words.length,
      right = List())
  }
}

case class Parsing(
  text: String,
  words: List[Word],
  queue: ListBuffer[ParsingFrame],
) {

  def nextLinearTree(): Option[List[LinearTreePart]] = {
    var result: Option[List[LinearTreePart]] = None
    var continue: Boolean = true

    while (continue) {

      // right side
      queue.headOption match {
        case None => continue = false
        case Some(frame) => {
          queue.trimStart(1)

          if (frame.remain.isEmpty) {
            if (frame.leftIndex == frame.rightIndex) {
              result = Some(frame.left ++ frame.right)
              continue = false
            }
          } else {
            val head = frame.remain.head
            val rest = frame.remain.tail

            head match {
              case LinearTreePartStr(str) =>
                val word = words(frame.leftIndex)
                if (word.str == str) {
                  queue.prepend(frame.copy(
                    leftIndex = frame.leftIndex + 1,
                    left = frame.left :+ head,
                    remain = rest))
                }
              case LinearTreePartRule(rule) =>
                rule.choices
                  .map { case (choiceName, ruleParts) =>
                    List(LinearTreePartBra(rule, choiceName)) ++
                    ruleParts.map(LinearTreePart.fromRulePart) ++
                    List(LinearTreePartKet(rule, choiceName)) }
                  .filter { case parts =>
                    val bound = parts.map(_.lowerBound).sum + rest.map(_.lowerBound).sum
                    bound <= frame.rightIndex - frame.leftIndex }
                  .foreach { case parts =>
                    // one look ahead
                    // TODO should not do this manually
                    val head = parts.head
                    head match {
                      case LinearTreePartStr(str) =>
                        val word = words(frame.leftIndex)
                        if (word.str == str) {
                          queue.prepend(frame.copy(
                            leftIndex = frame.leftIndex + 1,
                            left = frame.left :+ head,
                            remain = parts.tail ++ rest))
                        }
                      case LinearTreePartRule(rule) =>
                        queue.append(frame.copy(remain = parts ++ rest))
                      case LinearTreePartBra(rule, choiceName) =>
                        queue.prepend(frame.copy(
                          left = frame.left :+ head,
                          remain = parts.tail ++ rest))
                      case LinearTreePartKet(rule, choiceName) =>
                        queue.prepend(frame.copy(
                          left = frame.left :+ head,
                          remain = parts.tail ++ rest))
                      case LinearTreePartPred(pred) =>
                        val word = words(frame.leftIndex)
                        if (pred(word.str)) {
                          queue.prepend(frame.copy(
                            leftIndex = frame.leftIndex + 1,
                            left = frame.left :+ LinearTreePartStr(word.str),
                            remain = parts.tail ++ rest))
                        }
                    }
                  }
              case LinearTreePartBra(rule, choiceName) =>
                queue.prepend(frame.copy(left = frame.left :+ head, remain = rest))
              case LinearTreePartKet(rule, choiceName) =>
                queue.prepend(frame.copy(left = frame.left :+ head, remain = rest))
              case LinearTreePartPred(pred) =>
                val word = words(frame.leftIndex)
                if (pred(word.str)) {
                  queue.prepend(frame.copy(
                    leftIndex = frame.leftIndex + 1,
                    left = frame.left :+ LinearTreePartStr(word.str),
                    remain = rest))
                }
            }
          }
        }
      }

      // // left side

      // queue.headOption match {
      //   case None => continue = false
      //   case Some(frame) => {
      //     queue.trimStart(1)

      //     if (frame.remain.isEmpty) {
      //       if (frame.leftIndex == frame.rightIndex) {
      //         result = Some(frame.left ++ frame.right)
      //         continue = false
      //       }
      //     } else {
      //       val last = frame.remain.last
      //       val rest = frame.remain.init

      //       last match {
      //         case LinearTreePartStr(str) =>
      //           val word = words(frame.rightIndex - 1)
      //           if (word.str == str) {
      //             queue.prepend(frame.copy(
      //               remain = rest,
      //               rightIndex = frame.rightIndex - 1,
      //               right = last +: frame.right))
      //           }
      //         case LinearTreePartRule(rule) =>
      //           val linearTreeList = rule.choices
      //             .map { case (choiceName, ruleParts) =>
      //               List(LinearTreePartBra(rule, choiceName)) ++
      //               ruleParts.map(LinearTreePart.fromRulePart) ++
      //               List(LinearTreePartKet(rule, choiceName)) }
      //             .filter { case parts =>
      //               val bound = parts.map(_.lowerBound).sum + rest.map(_.lowerBound).sum
      //               bound <= frame.rightIndex - frame.leftIndex }
      //             .map { case parts =>
      //               frame.copy(remain = rest ++ parts) }
      //           queue.appendAll(linearTreeList)
      //           // queue.prependAll(linearTreeList)
      //         case LinearTreePartBra(rule, choiceName) =>
      //           queue.prepend(frame.copy(right = last +: frame.right, remain = rest))
      //         case LinearTreePartKet(rule, choiceName) =>
      //           queue.prepend(frame.copy(right = last +: frame.right, remain = rest))
      //         case LinearTreePartPred(pred) =>
      //           val word = words(frame.rightIndex - 1)
      //           if (pred(word.str)) {
      //             queue.prepend(frame.copy(
      //               rightIndex = frame.rightIndex - 1,
      //               right = LinearTreePartStr(word.str) +: frame.right,
      //               remain = rest))
      //           }
      //       }
      //     }
      //   }
      // }

    }

    result
  }

  def nextTree(): Option[Tree] = {
    nextLinearTree().flatMap { case parts =>
      Some(Tree.fromLinearTree(parts))
    }
  }
}
