package xieyuheng.untyped

case class Apply (
  rator: Exp,
  rand: Exp,
) extends Exp {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      fn <- rator.eval(env)
      arg <- rand.eval(env)
      result <- fn match {
        case fn: Closure =>
          fn.body.eval(fn.env.ext(fn.name, arg))
        case fn: Neutral =>
          Right(NeutralApply(fn, arg))
        case _ =>
          Left(ErrorMsg(s"unknown fn value type: ${fn}"))
      }
    } yield result
  }
}
