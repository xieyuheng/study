package xieyuheng.partech

import scala.collection.mutable.ListBuffer

object LL_1_with_depth_first_search {
  case class Frame(
    leftIndex: Int,
    left: List[LinearTreePart],
    remain: List[LinearTreePart],
    rightIndex: Int,
    right: List[LinearTreePart])

  object Frame {
    def init(words: List[Word], rule: Rule): Frame = {
      Frame(
        leftIndex = 0,
        left = List(),
        remain = List(LinearTreePartRule(rule)),
        rightIndex = words.length,
        right = List())
    }
  }

  def init(words: List[Word], rule: Rule): LL_1_with_depth_first_search = {
    LL_1_with_depth_first_search(words, ListBuffer(Frame.init(words, rule)))
  }
}

case class LL_1_with_depth_first_search(
  words: List[Word],
  queue: ListBuffer[LL_1_with_depth_first_search.Frame],
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
    }

    result
  }

  def nextTree(): Option[Tree] = {
    nextLinearTree().flatMap { case parts =>
      Some(Tree.fromLinearTree(parts))
    }
  }
}
