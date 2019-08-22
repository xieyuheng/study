package xieyuheng.untyped

case class Apply (
  rator: Exp,
  rand: Exp,
) extends Exp {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      fun <- rator.eval(env)
      arg <- rand.eval(env)
      result <- fun match {
        case fun: Closure =>
          fun.body.eval(fun.env.ext(fun.name, arg))
        case fun: Neutral =>
          Right(NeutralApply(fun, arg))
        case _ =>
          Left(ErrorMsg(s"unknown fun value type: ${fun}"))
      }
    } yield result
  }
}
