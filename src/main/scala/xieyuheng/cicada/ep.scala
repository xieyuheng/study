package xieyuheng.cicada

import xieyuheng.cicada.dsl._

/** eval and print of repl */

object ep {
  def apply(exp: Exp)(implicit env: Env): Unit = {
    eval(exp, env) match {
      case Right(value) =>
        println(s"=> ${Pretty.Value(value)}")
      case Left(errorMsg) =>
        println(s"?> ${errorMsg}")
    }
  }
}
