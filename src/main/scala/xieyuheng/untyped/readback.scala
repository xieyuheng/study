package xieyuheng.untyped

object readback {

  def apply(value: Val, used_names: Set[String]): Either[Err, Exp] =
    readback_val(value: Val, used_names: Set[String])

  def readback_val(value: Val, used_names: Set[String]): Either[Err, Exp] = {
    value match {
      case Closure(env: Env, name: String, body: Exp) =>
        val fresh_name = util.freshen (used_names, name)
        for {
          value <- eval(body, env.ext(name, NeuVar(fresh_name)))
          body2 <- readback_val(value, used_names + fresh_name)
        } yield Lambda(fresh_name, body2)
      case NeuVar(name: String) =>
        Right(Var(name))
      case NeuAp(fn: Neu, arg: Val) =>
        for {
          rator <- readback(fn, used_names)
          rand <- readback(arg, used_names)
        } yield Ap(rator, rand)
    }
  }

}
