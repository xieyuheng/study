package xieyuheng.eopl.lang_nameless

import pretty._

import xieyuheng.eopl.Interpreter
import xieyuheng.eopl.util._

import xieyuheng.partech.Parser

object lang_nameless extends Interpreter(
  "lang_nameless", "0.0.1", { case code =>
    Parser(grammar.lexer, grammar.exp).parse(code) match {
      case Right(tree) =>
        val idx_env = IdxEnv()
        val idx_ctx = IdxCtx()
        val exp = grammar.exp_matcher(tree)
        tran.tran_nameless(exp, idx_ctx) match {
          case Right(idx) =>
            eval.eval_idx(idx, idx_env) match {
              case Right(value) =>
                println(s">>> ${pretty_exp(exp)}")
                println(s">>> ${pretty_idx(idx)}")
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
