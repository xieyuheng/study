package xieyuheng.partech

import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

case class Parser(rule: Rule) {
  def parsing(text: String): Parsing = {
    Parsing(text, ListBuffer((0, LinearParseTree.empty, LinearParseTree.fromRule(rule))))
  }

  def parse(text: String): Option[LinearParseTree] = {
    parsing(text).nextLinearParseTree
  }
}

case class Parsing(
  text: String,
  queue: ListBuffer[(Int, LinearParseTree, LinearParseTree)] = ListBuffer(),
) {

  def nextLinearParseTree(): Option[LinearParseTree] = {
    var result: Option[LinearParseTree] = None

    breakable {
      while (true) {
        queue.headOption match {
          case Some((i, left, right)) => {
            val (leftText, rightText) = text.splitAt(i)
            queue.trimStart(1)
            if (right.complete()) {
              if (right.toStrWithoutVar == rightText) {
                result = Some(left.append(right))
                break
              }
            } else {
              val frames = right.expend()
                .map { case tree => tree.shift() }
                .filter { case (mid, right) =>
                  val midStr = mid.toStrWithoutVar
                  val rightStr = right.toStrWithoutVar
                  rightText.startsWith(midStr) &&
                  rightStr.length <= rightText.length - midStr.length
                  // TODO
                  // improve pruning by rightStr
                }
                .map { case (mid, right) =>
                  (i + mid.toStrWithoutVar.length,
                    left.append(mid),
                    right)
                }
              queue.appendAll(frames)
            }
          }
          case None => break
        }
      }
    }

    result
  }
}
