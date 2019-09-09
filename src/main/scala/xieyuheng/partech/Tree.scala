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
  def fromLinearTree(parts: List[LinearTreePart]): Tree = {
    var stack: ListBuffer[Tree] = ListBuffer()
    var marks: ListBuffer[Int] = ListBuffer()

    parts.reverse.foreach { case part =>
      part match {
        case LinearTreePartStr(str) =>
          stack.prepend(Leaf(str))
        case LinearTreePartRule(rule) =>
          throw new Exception(s"var: ${rule.name}, in linearTree: ${pretty.prettyLinearTree(parts)}")
        case LinearTreePartBra(rule, choiceName) =>
          val count = marks.head
          marks.trimStart(1)
          val children = stack.dropRight(count)
          stack.trimStart(children.length)
          stack.prepend(Node(rule, choiceName, children.toList))
        case LinearTreePartKet(rule, choiceName) =>
          marks.prepend(stack.length)
        case LinearTreePartPred(pred) =>
          throw new Exception(s"pred: ${pred}, in linearTree: ${pretty.prettyLinearTree(parts)}")
      }
    }

    assert(stack.length == 1)

    stack(0)
  }

  // def to[A](tree: Tree)(implicit treeTo: TreeTo[A]): A = {
  //   treeTo(tree)
  // }
}
