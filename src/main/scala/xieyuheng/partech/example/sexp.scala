package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object sexp extends ExampleRule {

  def lexer = Lexer.default

  def sentences = List(
    "()",
    "( )",
    "(a b c)",
    "n",
    "(a b (c))",
    "(((a)) b (c))",
    "(true false)",
    "(true false true)",
    "(true ((((false)))))",
    "( true false)",
    "(true false true )",
    "(true ((( (false)))))",
  )

  def non_sentences = List(
    "(",
    "())",
    "true [false]",
    "true false",
  )

  def identifier = WordPred (
    "identifier", { case word =>
      word.headOption match {
        case Some(char) =>
          val head_set = lower_case_char_set ++ upper_case_char_set + '_'
          val tail_set = head_set ++ digit_char_set
          head_set.contains(char) && wordInCharSet(tail_set)(word.tail)
        case None => false
      }
    })

  def start = sexp

  def treeToMainType = Some(Sexp.treeToSexp)

  def sexp: Rule = Rule(
    "sexp", Map(
      "null" -> List("(", ")"),
      "atom" -> List(identifier),
      "sexp_list" -> List("(", non_empty_list(sexp), ")"),
    ))


  sealed trait Sexp
  final case object NullSexp extends Sexp
  final case class AtomSexp(str: String) extends Sexp
  final case class ConsSexp(car: Sexp, cdr: Sexp) extends Sexp

  object Sexp {
    implicit def treeToSexp: TreeTo[Sexp] = TreeTo[Sexp] { case tree =>
      tree match {
        case Node(Rule("sexp", _, _), "null", _) =>
          NullSexp
        case Node(Rule("sexp", _, _), "atom", List(Leaf(str))) =>
          AtomSexp(str)
        case Node(Rule("sexp", _, _), "sexp_list", List(_, list, _)) =>
          var sexp: Sexp = NullSexp
          val sexp_list = Tree.to[List[Sexp]](list)
          sexp_list.reverse.foreach { case car =>
            sexp = ConsSexp(car, sexp)
          }
          sexp
        case _ => throw new Exception()
      }
    }
  }
}
