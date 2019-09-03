package xieyuheng.partech

import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

case class Generator(rule: Rule) {
  def generate(): Generating = {
    Generating(ListBuffer(LinearTree.fromRule(rule)))
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
  queue: ListBuffer[LinearTree] = ListBuffer(),
) {

  def nextLinearTree(): Option[LinearTree] = {
    var result: Option[LinearTree] = None

    breakable {
      while (true) {
        queue.headOption match {
          case Some(tree) => {
            queue.trimStart(1)

            if (tree.complete()) {
              result = Some(tree)
              break
            } else {
              val treeList = tree.indexOfNextRule match {
                case -1 => List()
                case n => {
                  val LinearTreePartRule(rule) = tree.parts(n)
                  rule.choices.map { case (choiceName, ruleParts) =>
                    val newParts = {
                      List(LinearTreePartBra(rule, choiceName)) ++
                      ruleParts.map(LinearTreePart.fromRulePart) ++
                      List(LinearTreePartKet(rule, choiceName))
                    }
                    LinearTree(tree.parts.patch(n, newParts, 1))
                  }.toList
                }
              }
              queue.appendAll(treeList)
            }
          }
          case None => break
        }
      }
    }

    result
  }
}
