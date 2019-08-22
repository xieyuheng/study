package xieyuheng..systemT

case class Var (name: String) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    env.lookupValue(name) match {
      case Some(value) =>
        Right(value)
      case None =>
        Left(ErrorMsg(s"can not find var: ${this} in env"))
    }
  }

  /*
   ctx.lookupType(x) == T
   --------------------------
   ctx :- Var(x) => T
   */
  def infer(ctx: Ctx): Either[ErrorMsg, Type] = {
    ctx.lookupType(name) match {
      case Some(t) =>
        Right(t)
      case None =>
        Left(ErrorMsg(s"can not find var: ${this} in ctx"))
    }
  }
}
