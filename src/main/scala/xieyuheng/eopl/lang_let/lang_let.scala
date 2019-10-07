package xieyuheng.eopl.lang_let

import xieyuheng.eopl.Lang

import xieyuheng.partech.Parser

object lang_let extends Lang(
  "lang_let", "0.0.1", { case code =>
    Parser(grammar.lexer, grammar.exp).parse(code) match {
      case Right(tree) =>
        val env = EnvEmpty()
        val exp = grammar.exp_matcher(tree)
        eval.eval(exp, env) match {
          case Right(value) =>
            println(value)
          case Left(err) =>
            println(s"${err.msg}")
        System.exit(1)
        }
      case Left(error) =>
        println(s"[parse_error] ${error.msg}")
        System.exit(1)
    }
  }
)
