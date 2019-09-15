package xieyuheng.untyped

object eval {

  def apply(exp: Exp, env: Env): Either[Err, Val] =
    eval(exp: Exp, env: Env)

  def eval(exp: Exp, env: Env): Either[Err, Val] = {
    exp match {
      case Var(name) =>
        env.lookup_val(name) match {
          case Some(value) => Right(value)
          case None =>
            Left(Err(s"can not find var: ${name}"))
        }
      case Ap(rator: Exp, rand: Exp) =>
        for {
          fn <- eval(rator, env)
          arg <- eval(rand, env)
          result <- fn match {
            case fn: Closure =>
              eval(fn.body, fn.env.ext(fn.name, arg))
            case fn: Neu =>
              Right(NeuAp(fn, arg))
            case _ =>
              Left(Err(s"unknown fn value type: ${fn}"))
          }
        } yield result
      case Lambda(name: String, body: Exp) =>
        Right(Closure(env, name, body))
    }
  }
}
