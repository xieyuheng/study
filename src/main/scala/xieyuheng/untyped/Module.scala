package xieyuheng.untyped

case class Module(var env: Env = Env()) {
  def define(name: String, exp: Exp): Module = {
    eval(exp, env) match {
      case Right(value) =>
        env = env.ext(name, value)
      case Left(Err(msg)) =>
        println(msg)
    }
    this
  }

  def run(exp: Exp): Either[Err, Exp] = {
    val result = for {
      value <- eval(exp, env)
      norm <- readback(value, Set())
    } yield norm

    result match {
      case Right(exp) =>
        println(s"exp: ${exp}")
      case Left(Err(msg)) =>
        println(s"error: ${msg}")
    }

    result
  }
}
