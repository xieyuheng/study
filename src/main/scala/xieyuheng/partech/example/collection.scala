package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.dsl._

object collection {

  def non_empty_list(a: Rule)(implicit separater: String): Rule = Rule(
    s"non_empty_list", Map(
      "one" -> Seq(a),
      "more" -> Seq(a, separater, non_empty_list(a))),
    args = Map("a" -> a))

  implicit def tree_to_non_empty_list[A]
    (implicit treeToA: TreeTo[A])
      : TreeTo[List[A]] = TreeTo[List[A]] { case tree =>
      tree match {
        case Node(Rule("non_empty_list", _, _), "one", Seq(a)) =>
          List(Tree.to[A](a))
        case Node(Rule("non_empty_list", _, _), "more", Seq(a, _, tail)) =>
          Tree.to[A](a) :: Tree.to[List[A]](tail)
        case _ => throw new Exception()
      }
  }

}
