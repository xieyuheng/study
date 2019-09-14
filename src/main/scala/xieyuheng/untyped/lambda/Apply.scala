package xieyuheng.untyped

case class Ap (
  rator: Exp,
  rand: Exp,
) extends Exp {
  def eval(env: Env): Either[Err, Val] = {
    for {
      fn <- rator.eval(env)
      arg <- rand.eval(env)
      result <- fn match {
        case fn: Closure =>
          fn.body.eval(fn.env.ext(fn.name, arg))
        case fn: Neu =>
          Right(NeuAp(fn, arg))
        case _ =>
          Left(Err(s"unknown fn value type: ${fn}"))
      }
    } yield result
  }
}
