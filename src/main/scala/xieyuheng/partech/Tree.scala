package xieyuheng.partech

import scala.collection.mutable.ListBuffer

sealed trait Tree
final case class Leaf(str: String) extends Tree
final case class Node(
  rule: Rule,
  choiceName: String,
  children: List[Tree],
) extends Tree

object Tree {
  def to[A](tree: Tree)(implicit treeTo: TreeTo[A]): A = {
    treeTo(tree)
  }

  def matcher[A](
    name: String,
    map: Map[String, List[Tree] => A],
  ): Tree => A = {
    case Node(rule, choiceName, children) =>
      if (rule.name == name) {
        map.get(choiceName) match {
          case Some(f) => f(children)
          case None => throw new Exception()
        }
      } else {
        throw new Exception()
      }
    case _ => throw new Exception()
  }

}
