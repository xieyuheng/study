package xieyuheng.partech

import scala.collection.mutable.ListBuffer

object Earley {
  case class Frame()
}
case class Earley(
  words: List[Word],
  queue: ListBuffer[Earley.Frame],
) {

  def nextLinearTree(): Option[List[LinearTreePart]] = {
    ???
  }

  def nextTree(): Option[Tree] = {
    nextLinearTree().flatMap { case parts =>
      Some(Tree.fromLinearTree(parts))
    }
  }
}
