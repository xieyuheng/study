package xieyuheng..untyped

case class Var (
  name: String,
) extends Exp {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    env.lookupValue(name) match {
      case Some(value) => Right(value)
      case None =>
        Left(ErrorMsg(s"can not find var: ${name}"))
    }
  }
}
