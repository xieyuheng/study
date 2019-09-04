package xieyuheng.partech.lexerless

import scala.collection.mutable.ListBuffer

sealed trait Tree
final case class Leaf(str: String) extends Tree
final case class Node(
  rule: Rule,
  choiceName: String,
  children: Seq[Tree],
) extends Tree

object Tree {
  def fromLinearTree(linearTree: LinearTree): Tree = {
    assert(linearTree.complete())

    var stack: ListBuffer[Tree] = ListBuffer()
    var marks: ListBuffer[Int] = ListBuffer()

    linearTree.parts.reverse.foreach { case part =>
      part match {
        case LinearTreePartStr(str) =>
          stack.prepend(Leaf(str))
        case LinearTreePartRule(rule) =>
          throw new Exception(s"var: ${rule.name}, in linearTree: ${linearTree}")
        case LinearTreePartBra(rule, choiceName) =>
          val count = marks.head
          marks.trimStart(1)
          val children = stack.dropRight(count)
          stack.trimStart(children.length)
          stack.prepend(Node(rule, choiceName, children.toList))
        case LinearTreePartKet(rule, choiceName) =>
          marks.prepend(stack.length)
        case LinearTreePartPred(strPred) =>
          throw new Exception(s"strPred: ${strPred}, in linearTree: ${linearTree}")
      }
    }

    assert(stack.length == 1)

    stack(0)
  }

  def to[A](tree: Tree)(implicit treeTo: TreeTo[A]): A = {
    treeTo(tree)
  }
}
