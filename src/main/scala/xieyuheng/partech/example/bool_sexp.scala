package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.dsl._
import xieyuheng.partech.example.collection._

object bool_sexp {

  def bool = Rule(
    "bool", Map(
      "true" -> Seq("true"),
      "false" -> Seq("false")))

  sealed trait Bool
  final case object True extends Bool
  final case object False extends Bool

  object Bool {
    implicit def treeToBool = TreeTo[Bool] { case tree =>
      tree match {
        case Node(Rule("bool", _, _), "true", Seq(Leaf("true"))) =>
          True
        case Node(Rule("bool", _, _), "false", Seq(Leaf("false"))) =>
          False
        case _ => throw new Exception()
      }
    }
  }

  def bool_sexp = Rule(
    "bool_sexp", Map(
      "list" -> Seq("(", bool_sexp_list, ")"),
      "bool" -> Seq(bool)))

  sealed trait BoolSexp
  final case class BoolSexpList(list: List[BoolSexp]) extends BoolSexp
  final case class BoolSexpBool(bool: Bool) extends BoolSexp

  object BoolSexp {
    implicit def treeToBoolSexp: TreeTo[BoolSexp] = TreeTo[BoolSexp] { case tree =>
      tree match {
        case Node(Rule("bool_sexp", _, _), "list", Seq(_, list, _)) =>
          BoolSexpList(Tree.to[List[BoolSexp]](list))
        case Node(Rule("bool_sexp", _, _), "bool", Seq(bool)) =>
          BoolSexpBool(Tree.to[Bool](bool))
        case _ => throw new Exception()
      }
    }
  }

  // def bool_sexp_list: Rule = Rule(
  //   "bool_sexp_list", Map(
  //     "one" -> Seq(bool_sexp),
  //     "more" -> Seq(bool_sexp, " ", bool_sexp_list)))

  def bool_sexp_list: Rule = non_empty_list(bool_sexp)(" ")

}
