package xieyuheng.partech

import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

object Earley {
  case class Item(rule: Rule, choiceName: String, parts: List[RulePart], index: Int)
}

case class Earley(
  words: List[Word],
  start: Rule,
) {
  import Earley._

  var active: ArrayBuffer[Set[Item]] = ArrayBuffer.fill(words.length)(Set())
  var completed: ArrayBuffer[Set[Item]] = ArrayBuffer.fill(words.length)(Set())

  def predict(rule: Rule, index: Int): Unit = {
    rule.choices.foreach { case (name, parts) =>
      ???
    }
  }

  // def scan
  // def complete

  def run(): Unit = {
    predict(start, 0)
  }

  val recognize: Boolean = {
    ???
  }

  def nextTree(): Option[Tree] = {
    ???
  }
}
