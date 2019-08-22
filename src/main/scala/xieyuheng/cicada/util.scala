package xieyuheng.cicada

import xieyuheng.cicada.dsl._
import xieyuheng.cicada.pretty._

object util {

  def evalPrint(exp: Exp)(implicit env: Env): Unit = {
    eval(exp, env) match {
      case Right(value) =>
        println(s"=> ${prettyValue(value)}")
      case Left(errorMsg) =>
        println(s"?> ${errorMsg}")
    }
  }

  def evalOnRight[A](exp: Exp)(f: Value => A)(implicit env: Env): A = {
    eval(exp, env) match {
      case Right(value) => f(value)
      case Left(errorMsg) =>
        throw new Exception(s"${errorMsg}")
    }
  }
}
