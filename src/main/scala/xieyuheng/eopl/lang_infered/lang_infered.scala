package xieyuheng.eopl.lang_infered

import pretty._

import xieyuheng.util.mini_interpreter
import xieyuheng.util.err._

import xieyuheng.partech.Parser

object lang_infered extends mini_interpreter(
  "lang_infered", "0.0.1", { case code =>
    Parser(grammar.lexer, grammar.exp).parse(code) match {
      case Right(tree) =>
        val env = EnvEmpty()
        val ctx = Ctx()
        val bind = Bind()
        val exp = grammar.exp_matcher(tree)
        infer.infer(bind, ctx, exp) match {
          case Right(_) =>
            eval.eval(exp, env) match {
              case Right(value) =>
                println(s">>> ${pretty_exp(exp)}")
                println(s"=== ${pretty_val(value)}")
              case Left(err) =>
                println(s"${err.msg}")
                System.exit(1)
            }
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
