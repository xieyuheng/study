package xieyuheng.barendregt.de_bruijn

import xieyuheng.util.mini_interpreter

import xieyuheng.partech.Parser

object de_bruijn extends mini_interpreter (
  "de_bruijn", "0.0.1", { case code =>
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
