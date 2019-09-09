package xieyuheng.partech.lexerless.example

import xieyuheng.partech.lexerless._

trait ExampleRule {
  def sentences: List[String]
  def non_sentences: List[String]
  def start: Rule
  def treeToMainType: Option[TreeTo[_]]
}

object ExampleRule {
  val examples = List(
    bool_sexp,
    bool_sexp_non_empty,
    bool_sexp_with_space,
    tom_dick_and_harry,
    tdh,
    tdh_left,
    bin_sum,
    dec_sum,
    ab,
    abc,
  )
}
