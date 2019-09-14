package xieyuheng.systemt

case class Module(
  var env: Env = Env(),
  var ctx: Ctx = Ctx(),
) {
  def claim(name: String, t: Type): Module = {
    ctx = ctx.ext(name, t)
    this
  }

  def define(name: String, exp: Exp): Module = {
    ctx.lookupType(name) match {
      case Some(t) =>
        exp.check(ctx, t) match {
          case Right(()) =>
            for {
              value <- exp.eval(env)
            } yield {
              env = env.ext(name, value)
            }
          case Left(Err(errorMsg)) =>
            println(s"type check fail for name: ${name}, error: ${errorMsg}")
        }
      case None =>
        println(s"name: ${name} is not claimed before define")
    }
    this
  }

  def run(exp: Exp): Either[Err, Exp] = {
    val result = for {
      t <- exp.infer(ctx)
      value <- exp.eval(env)
      norm <- value.readback(ctx.names, t)
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
