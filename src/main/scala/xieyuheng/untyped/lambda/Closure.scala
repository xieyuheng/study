package xieyuheng.untyped

case class Closure (
  env: Env,
  name: String,
  body: Exp,
) extends Val {
  def readback(used_names: Set[String]): Either[Err, Exp] = {
    val fresh_name = util.freshen (used_names, name)
    for {
      value <- body.eval(env.ext(name, NeuVar(fresh_name)))
      body2 <- value.readback(used_names + fresh_name)
    } yield Lambda(fresh_name, body2)
  }
}
