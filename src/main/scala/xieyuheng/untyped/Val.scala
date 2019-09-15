package xieyuheng.untyped

sealed trait Val {
  def readback(used_names: Set[String]): Either[Err, Exp]
}

sealed case class Closure (
  env: Env,
  name: String,
  body: Exp,
) extends Val {
  def readback(used_names: Set[String]): Either[Err, Exp] = {
    val fresh_name = util.freshen (used_names, name)
    for {
      value <- eval(body, env.ext(name, NeuVar(fresh_name)))
      body2 <- value.readback(used_names + fresh_name)
    } yield Lambda(fresh_name, body2)
  }
}

sealed trait Neu extends Val

final case class NeuVar (
  name: String,
) extends Neu {
  def readback(_used_names: Set[String]): Either[Err, Exp] =
    Right(Var(name))
}

final case class NeuAp (
  fn: Neu,
  arg: Val,
) extends Neu {
  def readback(used_names: Set[String]): Either[Err, Exp] = {
    for {
      rator <- fn.readback (used_names)
      rand <- arg.readback (used_names)
    } yield Ap(rator, rand)
  }
}
