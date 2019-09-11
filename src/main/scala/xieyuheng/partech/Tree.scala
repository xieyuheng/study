package xieyuheng.partech

import xieyuheng.partech.pretty._

import scala.collection.mutable.ListBuffer

sealed trait Tree
final case class Leaf(str: String) extends Tree
final case class Node(
  rule: Rule,
  choiceName: String,
  children: List[Tree],
) extends Tree

object Tree {

  def matcher[A](
    name: String,
    map: Map[String, List[Tree] => A],
  ): Tree => A = { case tree =>
      tree match {
        case Node(rule, choiceName, children) =>
          if (rule.name == name) {
            map.get(choiceName) match {
              case Some(f) => f(children)
              case None =>
                println(s"matcher on ${name}, does not have choice: ${choiceName}")
                throw new Exception()
            }
          } else {
            println(s"matcher expecting rule: ${name}, but found: ${rule.name}:${choiceName}")
            println(s"tree: ${prettyTree(tree)}")
            throw new Exception()
          }
        case Leaf(str) =>
          println(s"matcher can not match leaf: ${Leaf(str)}")
          throw new Exception()
      }
  }
}
