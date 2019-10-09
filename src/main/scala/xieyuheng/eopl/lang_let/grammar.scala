package xieyuheng.eopl.lang_let

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "diff", "zero_p",
    "if", "then", "else",
    "let", "in",
  )

  def identifier = identifier_with_preserved("identifier", preserved)

  def exp: Rule = Rule(
    "exp", Map(
      "var" -> List(identifier),
      "num" -> List(digit),
      "minus_num" -> List("-", digit),
      "diff" -> List("diff", "(", exp, ",", exp, ")"),
      "zero_p" -> List("zero_p", "(", exp, ")"),
      "if" -> List("if", exp, "{", exp, "}", "else", "{", exp, "}"),
      "let" -> List("let", identifier, "=", exp, "in", exp),
      "block_one" -> List("{", exp, "}"),
    ))

  def exp_matcher: Tree => Exp = Tree.matcher[Exp](
    "exp", Map(
      "var" -> { case List(Leaf(name)) =>
        Var(name) },
      "num" -> { case List(Leaf(digit)) =>
        Num(digit.toInt) },
      "minus_num" -> { case List(_, Leaf(digit)) =>
        Num(-digit.toInt) },
      "diff" -> { case List(_, _, exp1, _, exp2, _) =>
        Diff(exp_matcher(exp1), exp_matcher(exp2))},
      "zero_p" -> { case List(_, _, exp1, _) =>
        ZeroP(exp_matcher(exp1)) },
      "if" -> { case List(_, exp1, _, exp2, _, _, _, exp3, _) =>
        If(exp_matcher(exp1), exp_matcher(exp2), exp_matcher(exp3))},
      "let" -> { case List(_, Leaf(name), _, exp1, _, body) =>
        Let(name, exp_matcher(exp1), exp_matcher(body))},
      "block_one" -> { case List(_, exp, _) =>
        exp_matcher(exp) },
    ))

}
