package xieyuheng.eopl.lang_letrec

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "diff", "zero_p",
    "if",
    "let", "rec", "and",
    "sole",
    "do",
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
      "let" -> List("let", identifier, "=", exp, exp),
      "fn" -> List("(", identifier, ")", "=", ">", exp),
      "ap" -> List(exp, "(", exp, ")"),
      "block_one" -> List("{", exp, "}"),
      "let_rec" -> List("let", "rec", identifier, "=", "(", identifier, ")", "=", ">", exp, exp),
      "let_rec_mutual" -> List(
        "let", "rec", identifier, "=", "(", identifier, ")", "=", ">", exp,
        non_empty_list(mutual_fn), exp),
      "sole" -> List("sole"),
      "do" -> List("do", exp, exp),
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
      "let" -> { case List(_, Leaf(name), _, exp1, body) =>
        Let(name, exp_matcher(exp1), exp_matcher(body))},
      "fn" -> { case List(_, Leaf(name), _, _, _, body) =>
        Fn(name, exp_matcher(body))},
      "ap" -> { case List(target, _, arg, _) =>
        Ap(exp_matcher(target), exp_matcher(arg)) },
      "block_one" -> { case List(_, exp, _) =>
        exp_matcher(exp) },
      "let_rec" -> { case List(_, _, Leaf(fn_name), _, _, Leaf(arg_name), _, _, _, fn_body, body) =>
        LetRec(fn_name, arg_name, exp_matcher(fn_body), exp_matcher(body))},
      "let_rec_mutual" -> { case List(
        _, _, Leaf(fn_name), _, _, Leaf(arg_name), _, _, _, fn_body,
        mutual_fn_list, body) =>
        val map = non_empty_list_matcher(mutual_fn_matcher)(mutual_fn_list).toMap
        val map2 = map + (fn_name -> (arg_name, exp_matcher(fn_body)))
        LetRecMutual(map2, exp_matcher(body))
      },
      "sole" -> { case List(_) => Sole() },
      "do" -> { case List(_, exp, body) => Do(exp_matcher(exp), exp_matcher(body)) },
    ))

  def mutual_fn: Rule = Rule(
    "mutual_fn", Map(
      "and" -> List("and", identifier, "=", "(", identifier, ")", "=", ">", exp),
    ))

  def mutual_fn_matcher: Tree => (String, (String, Exp)) = Tree.matcher[(String, (String, Exp))](
    "mutual_fn", Map(
      "and" -> { case List(_, Leaf(fn_name), _, _, Leaf(arg_name), _, _, _, fn_body) =>
        (fn_name, (arg_name, exp_matcher(fn_body))) },
    ))
}
