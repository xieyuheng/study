package xieyuheng.partech

import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

case class Generator(rule: Rule) {
  def generate(): Generating = {
    Generating(ListBuffer(LinearParseTree.fromRule(rule)))
  }

  def take(n: Int): List[LinearParseTree] = {
    val generating = generate()

    var count: Int = 0
    var list: List[LinearParseTree] = List()

    while (count < n) {
      count += 1
      generating.nextLinearParseTree() match {
        case Some(tree) => list = list ++ List(tree)
        case None => {}
      }
    }

    list
  }
}

case class Generating(
  queue: ListBuffer[LinearParseTree] = ListBuffer(),
) {

  def nextLinearParseTree(): Option[LinearParseTree] = {
    var result: Option[LinearParseTree] = None

    breakable {
      while (true) {
        queue.headOption match {
          case Some(tree) => {
            queue.trimStart(1)
            if (tree.complete()) {
              result = Some(tree)
              break
            } else {
              queue.appendAll(tree.expend())
            }
          }
          case None => break
        }
      }
    }

    result
  }
}
