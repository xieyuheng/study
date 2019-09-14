package xieyuheng.untyped

case class Closure (
  env: Env,
  name: String,
  body: Exp,
) extends Val {
  def readback(usedNames: Set[String]): Either[Err, Exp] = {
    val freshName = util.freshen (usedNames, name)
    for {
      value <- body.eval(env.ext(name, NeuVar(freshName)))
      body2 <- value.readback(usedNames + freshName)
    } yield Lambda(freshName, body2)
  }
}
