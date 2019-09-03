package xieyuheng.partech.example

import xieyuheng.partech._

trait ExampleRule {
  def sentences: Seq[String]
  def non_sentences: Seq[String]
  def main: Rule
}

object ExampleRule {
  val examples = Seq(
    bool_sexp,
    tom_dick_and_harry,
    tdh,
    tdh_left,
    bin_sum,
    dec_sum,
    ab,
    abc,
  )
}
