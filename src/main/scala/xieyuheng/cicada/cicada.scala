package xieyuheng.cicada

import pretty._
import eval._

import xieyuheng.util.mini_interpreter

import xieyuheng.partech.Parser

object cicada extends mini_interpreter (
  "cicada", "0.0.1", { case code =>
    Parser(grammar.lexer, grammar.exp).parse(code) match {
      case Right(tree) =>
        val exp = grammar.exp_matcher(tree)
        val env = Env()
        eval(env, exp) match {
          case Right(value) =>
            println(s"${pretty_value(value)}")
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
