package xieyuheng.untyped

case class Var (
  name: String,
) extends Exp {
  def eval(env: Env): Either[Err, Value] = {
    env.lookupValue(name) match {
      case Some(value) => Right(value)
      case None =>
        Left(Err(s"can not find var: ${name}"))
    }
  }
}
