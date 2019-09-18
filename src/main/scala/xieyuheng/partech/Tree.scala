package xieyuheng.partech

import xieyuheng.partech.pretty._

import scala.collection.mutable.ListBuffer

sealed trait Tree
final case class Leaf(str: String) extends Tree
final case class Node(
  rule: Rule,
  choice_name: String,
  children: List[Tree],
) extends Tree

object Tree {

  def matcher[A](
    name: String,
    map: Map[String, List[Tree] => A],
  ): Tree => A = { case tree =>
      tree match {
        case Node(rule, choice_name, children) =>
          if (rule.name == name) {
            map.get(choice_name) match {
              case Some(f) => f(children)
              case None =>
                println(s"matcher on ${name}, does not have choice: ${choice_name}")
                throw new Exception()
            }
          } else {
            println(s"matcher expecting rule: ${name}, but found: ${rule.name}:${choice_name}")
            println(s"tree: ${pretty_tree(tree)}")
            throw new Exception()
          }
        case Leaf(str) =>
          println(s"matcher can not match leaf: ${Leaf(str)}")
          throw new Exception()
      }
  }
}
