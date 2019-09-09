package xieyuheng.partech.lexerless.example

import xieyuheng.partech.lexerless._
import xieyuheng.partech.lexerless.ruleDSL._
import xieyuheng.partech.lexerless.predefined._

object bool_sexp_with_space extends ExampleRule {

  val sentences = List(
    "(true false)",
    "(true false true)",
    "(true ((((false)))))",
    "()",
    "( )",
    "( true false)",
    "(true false true )",
    "(true ((( (false)))))",
  )

  val non_sentences = List(
    "true [false]",
    "true false",
  )

  def start = bool_sexp

  def treeToMainType = Some(BoolSexp.treeToBoolSexp)

  def bool = Rule(
    "bool", Map(
      "true" -> List("true"),
      "false" -> List("false")))

  def bool_sexp = Rule(
    "bool_sexp", Map(
      "list" -> List("(", space, bool_sexp_list, space, ")"),
      "bool" -> List(bool)))

  def bool_sexp_list: Rule = list(bool_sexp)(" ")

  sealed trait Bool
  final case object True extends Bool
  final case object False extends Bool

  object Bool {
    implicit def treeToBool = TreeTo[Bool] { case tree =>
      tree match {
        case Node(Rule("bool", _, _), "true", List(Leaf("true"))) =>
          True
        case Node(Rule("bool", _, _), "false", List(Leaf("false"))) =>
          False
        case _ => throw new Exception()
      }
    }
  }

  sealed trait BoolSexp
  final case class BoolSexpList(list: List[BoolSexp]) extends BoolSexp
  final case class BoolSexpBool(bool: Bool) extends BoolSexp

  object BoolSexp {
    implicit def treeToBoolSexp: TreeTo[BoolSexp] = TreeTo[BoolSexp] { case tree =>
      tree match {
        case Node(Rule("bool_sexp", _, _), "list", List(_, _, list, _, _)) =>
          BoolSexpList(tree_to_list(Tree.to[BoolSexp])(list))
        case Node(Rule("bool_sexp", _, _), "bool", List(bool)) =>
          BoolSexpBool(Tree.to[Bool](bool))
        case _ => throw new Exception()
      }
    }
  }

}
