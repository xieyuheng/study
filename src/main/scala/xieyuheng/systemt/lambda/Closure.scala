package xieyuheng.systemt

case class Closure (
  env: Env,
  name: String,
  body: Exp,
) extends Val {
  def readback(used_names: Set[String], t: Type): Either[Err, Exp] = {
    t match {
      case Arrow(argType, retType) =>
        val fresh_name = util.freshen (used_names, name)
        for {
          value <- Ap.exe(this, TheNeu(argType, NeuVar(fresh_name)))
          body2 <- value.readback(used_names + fresh_name, retType)
        } yield Lambda(fresh_name, body2)
      case _ => Left(Err(
        s"type of lambda should be arrow: ${t}"))
    }
  }
}
