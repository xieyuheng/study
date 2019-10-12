package xieyuheng.lambda

import xieyuheng.util.mini_interpreter

import xieyuheng.partech.Parser

object lambda extends mini_interpreter (
  "lambda", "0.0.1", { case code =>
    Parser(grammar.lexer, grammar.module).parse(code) match {
      case Right(tree) =>
        val module = grammar.module_matcher(tree)
        module.run()
      case Left(error) =>
        println(s"[parse_error] ${error.msg}")
        System.exit(1)
    }
  }
)
