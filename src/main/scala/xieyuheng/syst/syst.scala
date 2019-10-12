package xieyuheng.syst

import xieyuheng.partech.Parser

import xieyuheng.util.mini_interpreter

object syst extends mini_interpreter (
  "syst", "0.0.1", { case code =>
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
