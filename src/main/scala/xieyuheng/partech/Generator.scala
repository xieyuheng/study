package xieyuheng.partech

import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

case class Generator(
  queue: ListBuffer[LinearParseTree] = ListBuffer(),
) {

  def nextLinearParseTree(): Option[LinearParseTree] = {
    var result: Option[LinearParseTree] = None

    breakable {
      while (true) {
        queue.headOption match {
          case Some(tree) => {
            if (tree.complete()) {
              queue.trimStart(1)
              result = Some(tree)
              break
            } else {
              queue.trimStart(1)
              queue.appendAll(tree.expend())
            }
          }
          case None => break
        }
      }
    }

    result
  }

  def take(n: Int): List[LinearParseTree] = {
    var count: Int = 0
    var list: List[LinearParseTree] = List()

    while (count < n) {
      count += 1
      nextLinearParseTree() match {
        case Some(tree) => list = list ++ List(tree)
        case None => {}
      }
    }

    list
  }
}

object Generator {
  def fromRule(rule: Rule): Generator = {
    val queue: ListBuffer[LinearParseTree] = ListBuffer(LinearParseTree.fromRule(rule))
    Generator(queue)
  }
}
