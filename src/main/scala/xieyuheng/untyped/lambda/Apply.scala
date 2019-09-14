package xieyuheng.untyped

case class Apply (
  rator: Exp,
  rand: Exp,
) extends Exp {
  def eval(env: Env): Either[Err, Value] = {
    for {
      fn <- rator.eval(env)
      arg <- rand.eval(env)
      result <- fn match {
        case fn: Closure =>
          fn.body.eval(fn.env.ext(fn.name, arg))
        case fn: Neutral =>
          Right(NeutralApply(fn, arg))
        case _ =>
          Left(Err(s"unknown fn value type: ${fn}"))
      }
    } yield result
  }
}
