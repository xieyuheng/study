package xieyuheng.untyped

case class Closure (
  env: Env,
  name: String,
  body: Exp,
) extends Value {
  def readback(usedNames: Set[String]): Either[Err, Exp] = {
    val freshName = Util.freshen (usedNames, name)
    for {
      value <- body.eval(env.ext(name, NeutralVar(freshName)))
      body2 <- value.readback(usedNames + freshName)
    } yield Lambda(freshName, body2)
  }
}
