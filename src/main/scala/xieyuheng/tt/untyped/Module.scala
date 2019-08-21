package xieyuheng.tt.untyped

case class Module(var env: Env = Env()) {
  def define(name: String, exp: Exp): Module = {
    exp.eval(env) match {
      case Right(value) =>
        env = env.ext(name, value)
      case Left(ErrorMsg(msg)) =>
        println(msg)
    }
    this
  }

  def run(exp: Exp): Either[ErrorMsg, Exp] = {
    val result = for {
      value <- exp.eval(env)
      norm <- value.readBack(Set())
    } yield norm

    result match {
      case Right(exp) =>
        println(s"exp: ${exp}")
      case Left(ErrorMsg(msg)) =>
        println(s"error: ${msg}")
    }

    result
  }
}
