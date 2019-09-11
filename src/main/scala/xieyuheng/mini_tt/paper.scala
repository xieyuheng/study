package xieyuheng.mini_tt

import xieyuheng.partech._
import xieyuheng.partech.pretty._
import xieyuheng.partech.example._

import xieyuheng.mini_tt.expDSL._

object paper_test extends App {

  val code = s"""
  let id: (A: U, A) -> A = (A, x) => x

  let bool_t: U = sum {
    true[];
    false[];
  }

  let true: bool_t = true[]
  let false: bool_t = false[]

  eq! id(bool_t, true) true
  eq! id(bool_t, false) false
  eq! id(bool_t, id) id

  eval! id(bool_t, true)
  eval! id(bool_t, false)
  eval! id(bool_t, id)
  """

  var module = Parser(grammar.lexer, grammar.module).parse(code) match {
    case Right(tree) => grammar.module_matcher(tree)
    case Left(error) =>
      println(s"[paper_test] parse error")
      println(s"- code: ${code}")
      println(s"- error: ${error}")
      throw new Exception()
  }

  module.run()
}
