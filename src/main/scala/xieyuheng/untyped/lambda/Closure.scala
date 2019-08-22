package xieyuheng.untyped

case class Closure (
  env: Env,
  name: String,
  body: Exp,
) extends Value {
  def readBack(usedNames: Set[String]): Either[ErrorMsg, Exp] = {
    val freshName = Util.freshen (usedNames, name)
    for {
      value <- body.eval(env.ext(name, NeutralVar(freshName)))
      body2 <- value.readBack(usedNames + freshName)
    } yield Lambda(freshName, body2)
  }
}
