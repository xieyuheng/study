package xieyuheng.adventure.jojo_untyped

import pretty._

import xieyuheng.util.mini_interpreter

import xieyuheng.partech.Parser

object jojo_untyped extends mini_interpreter (
  "jojo_untyped", "0.0.1", { case code =>
    Parser(grammar.lexer, grammar.jo_list).parse(code) match {
      case Right(tree) =>
        val jo_list = grammar.jo_list_matcher(tree)
        val frame = Frame(0, jo_list, EnvEmpty())
        val rs = Rs()
          .push(exe.frame_empty)
          .push(frame)
        val ds = Ds()
        exe.run(ds, rs) match {
          case Right(ds) =>
            println(pretty_ds(ds))
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
