package xieyuheng.eopl.lang_infered

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object grammar {

  val lexer = Lexer.default

  def preserved: List[String] = List(
    "diff", "zero_p",
    "if",
    "let", "rec", "and",
    "sole", "do",
    "assert_eq", "show",
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
      "fn_anno" -> List("(", identifier, ":", ty, ")", "=", ">", exp),
      "ap" -> List(exp, "(", exp, ")"),
      "block_one" -> List("{", exp, "}"),
      "let_rec" -> List(
        "let", "rec", identifier, "=", "(", identifier, ")", "=", ">", exp,
        exp),
      "let_rec_anno_ret" -> List(
        "let", "rec", identifier, "=", "(", identifier, ")", ":", ty, "=", ">", exp,
        exp),
      "let_rec_anno_arg" -> List(
        "let", "rec", identifier, "=", "(", identifier, ":", ty, ")", "=", ">", exp,
        exp),
      "let_rec_anno_both" -> List(
        "let", "rec", identifier, "=", "(", identifier, ":", ty, ")", ":", ty, "=", ">", exp,
        exp),
      "let_rec_mutual" -> List(
        "let", "rec", identifier, "=", "(", identifier, ")", "=", ">", exp,
        non_empty_list(mutual_fn), exp),
      "let_rec_mutual_anno_ret" -> List(
        "let", "rec", identifier, "=", "(", identifier, ")", ":", ty, "=", ">", exp,
        non_empty_list(mutual_fn), exp),
      "let_rec_mutual_anno_arg" -> List(
        "let", "rec", identifier, "=", "(", identifier, ":", ty, ")", "=", ">", exp,
        non_empty_list(mutual_fn), exp),
      "let_rec_mutual_anno_both" -> List(
        "let", "rec", identifier, "=", "(", identifier, ":", ty, ")", ":", ty, "=", ">", exp,
        non_empty_list(mutual_fn), exp),
      "sole" -> List("sole"),
      "do" -> List("do", exp, exp),
      "assert_eq" -> List("assert_eq", "(", exp, ",", exp, ")"),
      "show" -> List("show", "(", exp, ")"),
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
        Fn(name, None, exp_matcher(body))},
      "fn_anno" -> { case List(_, Leaf(name), _, arg_t, _, _, _, body) =>
        Fn(name, Some(ty_matcher(arg_t)), exp_matcher(body))},
      "ap" -> { case List(target, _, arg, _) =>
        Ap(exp_matcher(target), exp_matcher(arg)) },
      "block_one" -> { case List(_, exp, _) =>
        exp_matcher(exp) },
      "let_rec" -> { case List(
        _, _, Leaf(fn_name), _, _, Leaf(arg_name), _, _, _, fn_body,
        body) =>
        LetRec(fn_name, arg_name,
          None, None,
          exp_matcher(fn_body),
          exp_matcher(body))},
      "let_rec_anno_ret" -> { case List(
        _, _, Leaf(fn_name), _, _, Leaf(arg_name), _, _, ret_t, _, _, fn_body,
        body) =>
        LetRec(fn_name, arg_name,
          None, Some(ty_matcher(ret_t)),
          exp_matcher(fn_body),
          exp_matcher(body))},
      "let_rec_anno_arg" -> { case List(
        _, _, Leaf(fn_name), _, _, Leaf(arg_name), _, arg_t, _, _, _, fn_body,
        body) =>
        LetRec(fn_name, arg_name,
          Some(ty_matcher(arg_t)), None,
          exp_matcher(fn_body),
          exp_matcher(body))},
      "let_rec_anno_both" -> { case List(
        _, _, Leaf(fn_name), _, _, Leaf(arg_name), _, arg_t, _, _, ret_t, _, _, fn_body,
        body) =>
        LetRec(fn_name, arg_name,
          Some(ty_matcher(arg_t)), Some(ty_matcher(ret_t)),
          exp_matcher(fn_body),
          exp_matcher(body))},
      "let_rec_mutual" -> { case List(
        _, _, Leaf(fn_name), _, _, Leaf(arg_name), _, _, _, fn_body,
        mutual_fn_list, body) =>
        val map = non_empty_list_matcher(mutual_fn_matcher)(mutual_fn_list).toMap
        val map2 = map + (fn_name -> (
          arg_name, None, None,
          exp_matcher(fn_body)))
        LetRecMutual(map2, exp_matcher(body))
      },
      "let_rec_mutual_anno_ret" -> { case List(
        _, _, Leaf(fn_name), _, _, Leaf(arg_name), _, _, ret_t, _, _, fn_body,
        mutual_fn_list, body) =>
        val map = non_empty_list_matcher(mutual_fn_matcher)(mutual_fn_list).toMap
        val map2 = map ++ Map((fn_name, (
          arg_name, None, Some(ty_matcher(ret_t)),
          exp_matcher(fn_body))))
        LetRecMutual(map2, exp_matcher(body))
      },
      "let_rec_mutual_anno_arg" -> { case List(
        _, _, Leaf(fn_name), _, _, Leaf(arg_name), _, arg_t, _, _, _, fn_body,
        mutual_fn_list, body) =>
        val map = non_empty_list_matcher(mutual_fn_matcher)(mutual_fn_list).toMap
        val map2 = map ++ Map ((fn_name, (
          arg_name, Some(ty_matcher(arg_t)), None,
          exp_matcher(fn_body))))
        LetRecMutual(map2, exp_matcher(body))
      },
      "let_rec_mutual_anno_both" -> { case List(
        _, _, Leaf(fn_name), _, _, Leaf(arg_name), _, arg_t, _, _, ret_t, _, _, fn_body,
        mutual_fn_list, body) =>
        val map = non_empty_list_matcher(mutual_fn_matcher)(mutual_fn_list).toMap
        val map2 = map ++ Map ((fn_name, (
          arg_name, Some(ty_matcher(arg_t)), Some(ty_matcher(ret_t)),
          exp_matcher(fn_body))))
        LetRecMutual(map2, exp_matcher(body))
      },
      "sole" -> { case List(_) =>
        Sole() },
      "do" -> { case List(_, exp1, body) =>
        Do(exp_matcher(exp1), exp_matcher(body)) },
      "assert_eq" -> { case List(_, _, exp1, _, exp2, _) =>
        AssertEq(exp_matcher(exp1), exp_matcher(exp2)) },
      "show" -> { case List(_, _, exp1, _) =>
        Show(exp_matcher(exp1)) },
    ))

  def mutual_fn: Rule = Rule(
    "mutual_fn", Map(
      "and" -> List(
        "and", identifier, "=", "(", identifier, ")", "=", ">", exp),
      "and_anno_ret" -> List(
        "and", identifier, "=", "(", identifier, ")", ":", ty, "=", ">", exp),
      "and_anno_arg" -> List(
        "and", identifier, "=", "(", identifier, ":", ty, ")", "=", ">", exp),
      "and_anno_both" -> List(
        "and", identifier, "=", "(", identifier, ":", ty, ")", ":", ty, "=", ">", exp),
    ))

  def mutual_fn_matcher = Tree.matcher[(String, (String, Option[Type], Option[Type], Exp))](
    "mutual_fn", Map(
      "and" -> { case List(_, Leaf(fn_name), _, _, Leaf(arg_name), _, _, _, fn_body) =>
        (fn_name, (arg_name, None, None, exp_matcher(fn_body))) },
      "and_anno_ret" -> { case List(
        _, Leaf(fn_name), _, _, Leaf(arg_name), _, _, ret_t, _, _, fn_body) =>
        (fn_name, (arg_name, None, Some(ty_matcher(ret_t)), exp_matcher(fn_body))) },
      "and_anno_arg" -> { case List(
        _, Leaf(fn_name), _, _, Leaf(arg_name), _, arg_t, _, _, _, fn_body) =>
        (fn_name, (arg_name, Some(ty_matcher(arg_t)), None, exp_matcher(fn_body))) },
      "and_anno_both" -> { case List(
        _, Leaf(fn_name), _, _, Leaf(arg_name), _, arg_t, _, _, ret_t, _, _, fn_body) =>
        (fn_name, (arg_name, Some(ty_matcher(arg_t)), Some(ty_matcher(ret_t)), exp_matcher(fn_body))) },
    ))

  def ty: Rule = Rule(
    "ty", Map(
      "int_t" -> List("int_t"),
      "bool_t" -> List("bool_t"),
      "sole_t" -> List("sole_t"),
      "arrow" -> List("(", ty, ")", "-", ">", ty),
    ))

  def ty_matcher: Tree => Type = Tree.matcher[Type](
    "ty", Map(
      "int_t" -> { case List(_) =>
        TypeInt() },
      "bool_t" -> { case List(_) =>
        TypeBool() },
      "sole_t" -> { case List(_) =>
        TypeSole() },
      "arrow" -> { case List(_, arg_t, _, _, _, ret_t) =>
        TypeArrow(ty_matcher(arg_t), ty_matcher(ret_t)) },
    ))

}
