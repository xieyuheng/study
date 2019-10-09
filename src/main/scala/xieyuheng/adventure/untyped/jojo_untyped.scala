package xieyuheng.adventure.untyped

import pretty._

import xieyuheng.adventure.Interpreter
import xieyuheng.adventure.util._

import xieyuheng.partech.Parser

object jojo_untyped extends Interpreter(
  "jojo_untyped", "0.0.1", { case code =>
    Parser(grammar.lexer, grammar.jo_list).parse(code) match {
      case Right(tree) =>
        val jo_list = grammar.jo_list_matcher(tree)
        val env = EnvEmpty()
        val frame = Frame(0, jo_list, env)
        val rs = Rs(List(frame))
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
