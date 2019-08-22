package cicada

import cicada.dsl._
import cicada.pretty._

/** eval and print of repl */

object ep {
  def apply(exp: Exp)(implicit env: Env): Unit = {
    eval(exp, env) match {
      case Right(value) =>
        println(s"=> ${prettyValue(value)}")
      case Left(errorMsg) =>
        println(s"?> ${errorMsg}")
    }
  }
}
