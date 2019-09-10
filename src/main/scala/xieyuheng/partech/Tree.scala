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
  // def to[A](tree: Tree)(implicit treeTo: TreeTo[A]): A = {
  //   treeTo(tree)
  // }
}
