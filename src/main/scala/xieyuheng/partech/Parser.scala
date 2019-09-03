package xieyuheng.partech

import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

case class Parser(rule: Rule) {
  def parsing(text: String): Parsing = {
    Parsing(text, ListBuffer((0, LinearTree.empty, LinearTree.fromRule(rule))))
  }
}

case class Parsing(
  text: String,
  queue: ListBuffer[(Int, LinearTree, LinearTree)] = ListBuffer(),
) {

  def nextLinearTree(): Option[LinearTree] = {
    var result: Option[LinearTree] = None

    breakable {
      while (true) {
        queue.headOption match {
          case None => break
          case Some((i, left, right)) => {
            val (leftText, rightText) = text.splitAt(i)
            queue.trimStart(1)

            if (right.isEmpty) {
              if(rightText.isEmpty) {
                result = Some(left)
                break
              }
            } else {
              val head = right.head
              val tail = right.tail

              head match {
                case LinearTreePartStr(str) =>
                  if (rightText.startsWith(str)) {
                    queue.prepend((i + str.length, left.consEnd(head), tail))
                  }
                case LinearTreePartRule(rule) => {
                  val frames = rule.choices
                    .map { case (choiceName, ruleParts) =>
                      LinearTree(
                        List(LinearTreePartBra(rule, choiceName)) ++
                          ruleParts.map(LinearTreePart.fromRulePart) ++
                          List(LinearTreePartKet(rule, choiceName))) }
                    .filter { case newRight =>
                      // simply pruning by strLengthLowerBound
                      // - pruning is always heuristic,
                      // - pruning should balance with searching
                      newRight.strLengthLowerBound + tail.strLengthLowerBound <= rightText.length }
                    .map { case newRight => (i, left, newRight.append(tail)) }
                  queue.appendAll(frames)
                }
                case LinearTreePartBra(rule, choiceName) =>
                  queue.prepend((i, left.consEnd(head), tail))
                case LinearTreePartKet(rule, choiceName) =>
                  queue.prepend((i, left.consEnd(head), tail))
                case LinearTreePartPred(strPred) =>
                  if (strPred.check(rightText)) {
                    val str = rightText.take(strPred.length)
                    queue.prepend((i + strPred.length, left.consEnd(LinearTreePartStr(str)), tail))
                  }
              }
            }
          }
        }
      }
    }

    result
  }

  def nextTree(): Option[Tree] = {
    nextLinearTree().flatMap { case linearTree =>
      Some(Tree.fromLinearTree(linearTree))
    }
  }
}
