package xieyuheng.partech

import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

case class Generator(rule: Rule) {
  def generate(): Generating = {
    Generating(ListBuffer((LinearTree.empty, LinearTree.fromRule(rule))))
  }

  def take(n: Int): List[LinearTree] = {
    val generating = generate()

    var count: Int = 0
    var list: List[LinearTree] = List()

    while (count < n) {
      count += 1
      generating.nextLinearTree() match {
        case Some(tree) => list = list ++ List(tree)
        case None => {}
      }
    }

    list
  }
}

case class Generating(
  queue: ListBuffer[(LinearTree, LinearTree)] = ListBuffer(),
) {

  def nextLinearTree(): Option[LinearTree] = {
    var result: Option[LinearTree] = None

    breakable {
      while (true) {
        queue.headOption match {
          case None => break
          case Some((left, right)) => {
            queue.trimStart(1)

            if (right.isEmpty) {
              result = Some(left)
              break
            } else {
              val head = right.head
              val tail = right.tail

              head match {
                case LinearTreePartStr(str) =>
                  queue.prepend((left.consEnd(head), tail))
                case LinearTreePartRule(rule) => {
                  val treeList = rule.choices
                    .map { case (choiceName, ruleParts) =>
                      LinearTree(
                        List(LinearTreePartBra(rule, choiceName)) ++
                          ruleParts.map(LinearTreePart.fromRulePart) ++
                          List(LinearTreePartKet(rule, choiceName))) }
                    .map { case newRight => (left, newRight.append(tail)) }
                  queue.appendAll(treeList)
                }
                case LinearTreePartBra(rule, choiceName) =>
                  queue.prepend((left.consEnd(head), tail))
                case LinearTreePartKet(rule, choiceName) =>
                  queue.prepend((left.consEnd(head), tail))
                case LinearTreePartPred(strPred) =>
                  queue.prepend((left.consEnd(LinearTreePartStr(strPred.gen())), tail))
              }
            }
          }
        }
      }
    }

    result
  }
}
